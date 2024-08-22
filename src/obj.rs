//! A parser for Wavefront's `.obj` file format for storing 3D meshes.
use context::Context;
use lex::PeekableLexer;
use std::borrow::ToOwned;
use std::cmp::Ordering;
use std::cmp::Ordering::{Equal, Greater, Less};

use lex::{Lexer, ParseError};
use util::OrderingExt;

/// A set of objects, as listed in an `.obj` file.
#[derive(Clone, Debug, PartialEq)]
pub struct ObjSet {
  /// Which material library to use.
  pub material_library: Option<String>,
  /// The set of objects.
  pub objects: Vec<Object>,
}

/// A mesh object.
#[derive(Clone, Debug, PartialEq)]
pub struct Object {
  /// A human-readable name for this object.
  pub name: String,
  /// The set of vertices this object is composed of. These are referenced
  /// by index in `shapes` contained within each element of `geometry`.
  pub vertices: Vec<Vertex>,
  /// The set of texture vertices referenced by this object. The actual
  /// vertices are indexed by the second element in a `VTNIndex`.
  pub tex_vertices: Vec<TVertex>,
  /// The set of normals referenced by this object. This are are referenced
  /// by the third element in a `VTNIndex`.
  pub normals: Vec<Normal>,
  /// A set of shapes (with materials applied to them) of which this object is
  /// composed.
  pub geometry: Vec<Geometry>,
}

/// A set of shapes, all using the given material.
#[derive(Clone, Debug, PartialEq)]
pub struct Geometry {
  /// A reference to the material to apply to this geometry.
  pub material_name: Option<String>,
  /// The shapes of which this geometry is composed.
  pub shapes: Vec<Shape>,
}

/// A shape gathers a primitive and groups.
///
/// Each shape is associated with 0 or many groups. Those are text identifiers
/// used to gather geometry elements into different groups.
#[derive(Clone, Debug, PartialEq)]
pub struct Shape {
  /// The primitive of the shape.
  pub primitive: Primitive,
  /// Associated groups. No associated group means the shape uses the default
  /// group.
  pub groups: Vec<GroupName>,
  /// Associated smoothing groups. No associated smoothing group means the shape should be rendered
  /// flat.
  pub smoothing_groups: Vec<u32>,
}

/// Name of a group.
pub type GroupName = String;

/// The various primitives supported by this library.
///
/// Convex polygons more complicated than a triangle are automatically
/// converted into triangles.
#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum Primitive {
  /// A point specified by its position.
  Point(VTNIndex),
  /// A line specified by its endpoints.
  Line(VTNIndex, VTNIndex),
  /// A triangle specified by its three vertices.
  Triangle(VTNIndex, VTNIndex, VTNIndex),
}

/// A single 3-dimensional point on the corner of an object.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub struct Vertex {
  pub x: f64,
  pub y: f64,
  pub z: f64,
}

/// A single 3-dimensional normal
pub type Normal = Vertex;

/// A single 3-dimensional point on a texture. "Texure Vertex".
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub struct TVertex {
  pub u: f64,
  pub v: f64,
  pub w: f64,
}

fn fuzzy_cmp(a: f64, b: f64, delta: f64) -> Ordering {
  if (a - b).abs() <= delta {
    Equal
  } else if a < b {
    Less
  } else {
    Greater
  }
}

// TODO(cgaebel): Can we implement Eq here?
impl PartialEq for Vertex {
  fn eq(&self, other: &Vertex) -> bool {
    self.partial_cmp(other).unwrap() == Equal
  }
}

impl PartialOrd for Vertex {
  fn partial_cmp(&self, other: &Vertex) -> Option<Ordering> {
    Some(
      fuzzy_cmp(self.x, other.x, 0.00001)
        .lexico(|| fuzzy_cmp(self.y, other.y, 0.00001))
        .lexico(|| fuzzy_cmp(self.z, other.z, 0.00001)),
    )
  }
}

impl PartialEq for TVertex {
  fn eq(&self, other: &TVertex) -> bool {
    self.partial_cmp(other).unwrap() == Equal
  }
}

impl PartialOrd for TVertex {
  fn partial_cmp(&self, other: &TVertex) -> Option<Ordering> {
    Some(
      fuzzy_cmp(self.u, other.u, 0.00001)
        .lexico(|| fuzzy_cmp(self.v, other.v, 0.00001))
        .lexico(|| fuzzy_cmp(self.w, other.w, 0.00001)),
    )
  }
}

/// An index into the `vertices` array of an object, representing a vertex in
/// the mesh. After parsing, this is guaranteed to be a valid index into the
/// array, so unchecked indexing may be used.
pub type VertexIndex = usize;

/// An index into the `texture vertex` array of an object.
///
/// Unchecked indexing may be used, because the values are guaranteed to be in
/// range by the parser.
pub type TextureIndex = usize;

/// An index into the `normals` array of an object.
///
/// Unchecked indexing may be used, because the values are guaranteed to be in
/// range by the parser.
pub type NormalIndex = usize;

/// An index into the vertex array, with an optional index into the texture
/// array. This is used to define the corners of shapes which may or may not
/// be textured.
pub type VTNIndex = (VertexIndex, Option<TextureIndex>, Option<NormalIndex>);

/// Blender exports primitives as a list of the vertices representing their corners.
/// This function turns that into a set of OpenGL-usable shapes - i.e. points,
/// lines, or triangles.
fn to_triangles(xs: &[VTNIndex]) -> Vec<Primitive> {
  match xs.len() {
    0 => return vec![],
    1 => return vec![Primitive::Point(xs[0])],
    2 => return vec![Primitive::Line(xs[0], xs[1])],
    _ => {}
  }

  let last_elem = *xs.last().unwrap();

  xs[..xs.len() - 1]
    .iter()
    .zip(xs[1..xs.len() - 1].iter())
    .map(|(&x, &y)| Primitive::Triangle(last_elem, x, y))
    .collect()
}

#[derive(Clone)]
struct Parser<'a> {
  line_number: usize,
  lexer: PeekableLexer<'a>,
}

impl<'a> Parser<'a> {
  fn new(input: &'a str) -> Parser<'a> {
    Parser {
      line_number: 1,
      lexer: PeekableLexer::new(Lexer::new(input)),
    }
  }

  fn error_raw(&self, msg: String) -> ParseError {
    ParseError {
      line_number: self.line_number,
      message: msg,
    }
  }

  fn error<A, E>(&self, msg: E) -> Result<A, ParseError>
  where
    E: Into<String>,
  {
    Err(self.error_raw(msg.into()))
  }

  fn next(&mut self) -> Option<&'a str> {
    let ret = self.lexer.next_str();
    if let Some("\n") = ret {
      self.line_number += 1;
    }
    ret
  }

  fn advance(&mut self) {
    self.next();
  }

  fn peek(&mut self) -> Option<&'a str> {
    self.lexer.peek_str()
  }

  /// Take a parser function and try to parse with it. If the parser fails, `None` is returned and
  /// no input is consumed. If the parser succeeds, the input is consumed and the parser result
  /// is returned.
  ///
  /// Be careful while using this function, especially in recursive parsing as it might end up with
  /// non-linear parsing.
  fn try<P, T>(&mut self, parse: P) -> Option<T>
  where
    P: FnOnce(&mut Self) -> Result<T, ParseError>,
  {
    let mut tried = self.clone();

    match parse(&mut tried) {
      Ok(r) => {
        *self = tried;
        Some(r)
      }
      Err(_) => None,
    }
  }

  /// Possibly skips over some newlines.
  fn zero_or_more_newlines(&mut self) {
    while let Some("\n") = self.peek() {
      self.advance()
    }
  }

  /// Parse just a constant string.
  fn parse_tag(&mut self, tag: &str) -> Result<(), ParseError> {
    match self.next() {
      None => self.error(format!("Expected `{}` but got end of input.", tag)),
      Some(s) if s != tag => self.error(format!("Expected `{}` but got {}.", tag, s)),
      _ => Ok(()),
    }
  }

  fn parse_tag_or_eof(&mut self, tag: &str) -> Result<(), ParseError> {
    match self.next() {
      Some(s) if s != tag => self.error(format!("Expected `{}` or EOF but got {}.", tag, s)),
      _ => Ok(()),
    }
  }

  /// Skips over some newlines, failing if it didn't manage to skip any.
  fn one_or_more_newlines(&mut self) -> Result<(), ParseError> {
    self.parse_tag_or_eof("\n")?;
    self.zero_or_more_newlines();
    Ok(())
  }

  fn parse_str(&mut self) -> Result<&'a str, ParseError> {
    match self.next() {
      None => self.error(format!("Expected string but got end of input.")),
      Some("\n") => self.error(format!("Expected string but got `end of line`.")),
      Some(got) => Ok(got),
    }
  }

  fn parse_material_library(&mut self) -> Result<Option<&'a str>, ParseError> {
    match self.peek() {
      Some("mtllib") => {}
      _ => return Ok(None),
    }
    self.advance();
    self.parse_str().map(Some)
  }

  fn parse_object_name(&mut self) -> Result<&'a str, ParseError> {
    match self.peek() {
      Some("o") => {
        self.parse_tag("o")?;
        let name = self.parse_str();
        self.one_or_more_newlines()?;
        name
      }
      _ => Ok(""),
    }
  }

  // TODO(cgaebel): Should this be returning `num::rational::BigRational` instead?
  // I can't think of a good reason to do this except to make testing easier.
  fn parse_double(&mut self) -> Result<f64, ParseError> {
    let s = self.parse_str()?;
    lexical::parse(s).map_err(|_| self.error_raw(format!("Expected f64 but got {}.", s)))
  }

  fn parse_vertex(&mut self) -> Result<Vertex, ParseError> {
    self.parse_tag("v")?;

    let x = self.parse_double()?;
    let y = self.parse_double()?;
    let z = self.parse_double()?;

    Ok(Vertex { x, y, z })
  }

  fn parse_tex_vertex(&mut self) -> Result<TVertex, ParseError> {
    self.parse_tag("vt")?;
    let u = self.parse_double()?;

    match self.try(Self::parse_double) {
      Some(v) => {
        let w = self.try(Self::parse_double).unwrap_or(0.);
        Ok(TVertex { u, v, w })
      }
      None => Ok(TVertex { u, v: 0., w: 0. }),
    }
  }

  fn parse_normal(&mut self) -> Result<Normal, ParseError> {
    self.parse_tag("vn")?;

    let x = self.parse_double()?;
    let y = self.parse_double()?;
    let z = self.parse_double()?;

    Ok(Normal { x, y, z })
  }

  fn parse_usemtl(&mut self) -> Result<&'a str, ParseError> {
    self.parse_tag("usemtl")?;
    self.parse_str()
  }

  #[inline]
  fn parse_isize_from(&self, s: &str) -> Result<isize, ParseError> {
    lexical::parse(&s).map_err(|_| self.error_raw(format!("Expected isize but got {}.", s)))
  }

  fn parse_u32(&mut self) -> Result<u32, ParseError> {
    let s = self.parse_str()?;
    lexical::parse(&s).map_err(|_| self.error_raw(format!("Expected u32 but got {}.", s)))
  }

  fn parse_vtindex(
    &mut self,
    valid_vtx: (usize, usize),
    valid_tx: (usize, usize),
    valid_nx: (usize, usize),
  ) -> Result<VTNIndex, ParseError> {
    match self.next() {
      None => return self.error("Expected vertex index but got end of input.".to_owned()),
      Some(s) => {
        let process_split =
          |split: &str, valid_range: (usize, usize)| -> Result<Option<usize>, ParseError> {
            if split.len() > 0 {
              Ok(Some(self.check_valid_index(
                valid_range,
                self.parse_isize_from(split)?,
              )?))
            } else {
              Ok(None)
            }
          };

        let mut splits_iter = s.split('/');
        let split1 = splits_iter
          .next()
          .and_then(|s| process_split(&s, valid_tx).transpose())
          .transpose()?;
        let split2 = splits_iter
          .next()
          .and_then(|s| process_split(&s, valid_vtx).transpose())
          .transpose()?;
        let split3 = splits_iter
          .next()
          .and_then(|s| process_split(&s, valid_nx).transpose())
          .transpose()?;

        if split1.is_none() || splits_iter.next().is_some() {
          self.error(format!("Expected at least 1 and at most 3 vertex indexes."))
        } else {
          Ok((split1.unwrap(), split2, split3))
        }
      }
    }
  }

  /// `valid_values` is a range of valid bounds for the actual value.
  #[inline(always)]
  fn check_valid_index(
    &self,
    valid_values: (usize, usize),
    actual_value: isize,
  ) -> Result<usize, ParseError> {
    let (min, max) = valid_values;

    let mut x = actual_value;

    // Handle negative vertex indexes.
    if x < 0 {
      x = max as isize - x;
    }

    if x >= min as isize && x < max as isize {
      debug_assert!(x > 0);
      Ok((x - min as isize) as usize)
    } else {
      self.error(format!(
        "Expected index in the range [{}, {}), but got {}.",
        min, max, actual_value
      ))
    }
  }

  fn parse_face(
    &mut self,
    valid_vtx: (usize, usize),
    valid_tx: (usize, usize),
    valid_nx: (usize, usize),
    current_groups: &Vec<GroupName>,
    current_smoothing_groups: &Vec<u32>,
  ) -> Result<Vec<Shape>, ParseError> {
    match self.next() {
      Some("f") => {}
      Some("l") => {}
      None => return self.error("Expected `f` or `l` but got end of input.".to_owned()),
      Some(s) => return self.error(format!("Expected `f` or `l` but got {}.", s)),
    }

    let mut corner_list = Vec::new();

    corner_list.push(self.parse_vtindex(valid_vtx, valid_tx, valid_nx)?);

    loop {
      match self.peek() {
        None => break,
        Some("\n") => break,
        Some(_) => {
          let r = self.parse_vtindex(valid_vtx, valid_tx, valid_nx);
          match r {
            Ok(r) => {
              corner_list.push(r);
            }
            Err(e) => {
              println!("{:?}", e);
            }
          }
        }
      }
    }

    Ok(
      to_triangles(&corner_list)
        .into_iter()
        .map(|prim| Shape {
          primitive: prim,
          groups: current_groups.clone(),
          smoothing_groups: current_smoothing_groups.clone(),
        })
        .collect(),
    )
  }

  fn parse_objects(&mut self) -> Result<Vec<Object>, ParseError> {
    let mut context = Context::new();
    loop {
      match self.peek() {
        Some(t) => {
          match t {
            "#" => {
              self.one_or_more_newlines()?;
            }
            "\n" => {
              self.one_or_more_newlines()?;
            }
            "o" => {
              context.set_name(self.parse_object_name()?.to_string());
            }
            "usemtl" => {
              // 遇到新的mesh了
              context.set_material_name(self.parse_usemtl()?.to_string());
            }
            "v" => {
              context.add_vertices(self.parse_vertex()?);
            }
            "vt" => {
              context.add_tex_vertices(self.parse_tex_vertex()?);
            }
            "vn" => {
              context.add_normals(self.parse_normal()?);
            }
            "f" | "l" => {
              let t = self.parse_face(
                context.valid_vtx,
                context.valid_tx,
                context.valid_nx,
                &context.current_groups,
                &context.current_smoothing_groups,
              )?;
              context.add_face(t);
            }
            "g" => {
              self.advance();
              context.set_current_groups(self.parse_groups()?);
            }
            "s" => {
              self.advance();
              context.set_current_smoothing_groups(self.parse_smoothing_groups()?);
            }
            _ => {
              println!("unknown tag: {}, line {}", t, self.line_number);
              self.advance();
            }
          }
        }
        None => break,
      }
      self.zero_or_more_newlines();
    }

    let result = context.finish();
    Ok(result)
  }

  fn parse_objset(&mut self) -> Result<ObjSet, ParseError> {
    self.zero_or_more_newlines();

    let material_library = self.parse_material_library()?;

    if material_library.is_some() {
      self.one_or_more_newlines()?;
    }

    let r = self.parse_objects();
    if r.is_err() {
      println!("parse_objects {:?}", r);
    }
    let objects = r.unwrap();

    self.zero_or_more_newlines();

    if let Some(s) = self.peek() {
      return self.error(format!("Expected end of input but got {}.", s));
    }

    Ok(ObjSet {
      material_library: material_library.map(|s| s.to_owned()),
      objects,
    })
  }

  fn parse_groups(&mut self) -> Result<Vec<GroupName>, ParseError> {
    let mut groups = Vec::new();

    loop {
      // ends the list of group names
      // g without any name is valid and means default group
      if let Some("\n") = self.peek() {
        break;
      }

      let name = self.parse_str()?;
      groups.push(name.to_owned());
    }

    Ok(groups)
  }

  fn parse_smoothing_groups(&mut self) -> Result<Vec<u32>, ParseError> {
    let mut groups = Vec::new();

    if self.try(|p| p.parse_tag("off")).is_none() {
      loop {
        let group = self.parse_u32()?;
        groups.push(group);

        if let Some("\n") = self.peek() {
          break;
        }
      }
    }

    Ok(groups)
  }
}

/// Parses a wavefront `.obj` file, returning either the successfully parsed
/// file, or an error. Support in this parser for the full file format is
/// best-effort and realistically I will only end up supporting the subset
/// of the file format which falls under the "things I see exported from blender"
/// category.
pub fn parse<S: AsRef<str>>(input: S) -> Result<ObjSet, ParseError> {
  Parser::new(input.as_ref()).parse_objset()
}
