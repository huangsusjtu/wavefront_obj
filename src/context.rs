use obj::{Geometry, GroupName, Normal, Object, Shape, TVertex, Vertex};

pub(crate) struct Context {
  vertices: Vec<Vertex>,
  normals: Vec<Normal>,
  tex_vertices: Vec<TVertex>,
  pub(crate) valid_vtx: (usize, usize),
  pub(crate) valid_tx: (usize, usize),
  pub(crate) valid_nx: (usize, usize),

  name: String, // object name
  material_name: String,
  pub(crate) current_groups: Vec<GroupName>,
  pub(crate) current_smoothing_groups: Vec<u32>,

  geometry_s: Vec<Geometry>,

  // flags
  is_in_face: bool,
  is_in_vertices: bool,
  is_in_normals: bool,
  is_in_tex_vertices: bool,

  // result
  pub(crate) objects: Vec<Object>,
}

impl Context {
  pub(crate) fn new() -> Self {
    Context {
      vertices: vec![],
      normals: vec![],
      tex_vertices: vec![],
      valid_vtx: (1, 1),
      valid_tx: (1, 1),
      valid_nx: (1, 1),
      name: "".to_string(),
      material_name: "".to_string(),
      current_groups: vec![],
      current_smoothing_groups: vec![],

      geometry_s: vec![],
      is_in_face: false,
      is_in_vertices: false,
      is_in_normals: false,
      is_in_tex_vertices: false,

      objects: vec![],
    }
  }

  pub(crate) fn finish(&mut self) -> Vec<Object> {
    if !self.geometry_s.is_empty() {
      self.objects.push(Object {
        name: self.name.to_owned(),
        vertices: std::mem::take(&mut self.vertices),
        tex_vertices: std::mem::take(&mut self.tex_vertices),
        normals: std::mem::take(&mut self.normals),
        geometry: std::mem::take(&mut self.geometry_s),
      });
    }

    return std::mem::take(&mut self.objects);
  }

  pub(crate) fn set_name(&mut self, name: String) {
    self.is_in_face = false;

    // check old
    if !self.geometry_s.is_empty() {
      self.objects.push(Object {
        name: self.name.to_owned(),
        vertices: self.vertices.clone(),
        tex_vertices: self.tex_vertices.clone(),
        normals: self.normals.clone(),
        geometry: self.geometry_s.clone(),
      });
    }
    self.geometry_s.clear();

    self.name = name;
  }

  pub(crate) fn set_material_name(&mut self, material_name: String) {
    self.material_name = material_name;
    self.is_in_face = false;
  }

  pub(crate) fn set_current_groups(&mut self, name: Vec<GroupName>) {
    self.current_groups = name;
  }

  pub(crate) fn set_current_smoothing_groups(&mut self, name: Vec<u32>) {
    self.current_smoothing_groups = name;
  }

  pub(crate) fn add_vertices(&mut self, v: Vertex) {
    if self.is_in_vertices == false {
      self.is_in_vertices = true;
      self.vertices.clear();
      self.valid_vtx.0 = self.valid_vtx.1;
    }
    self.vertices.push(v);
    self.valid_vtx.1 = self.valid_vtx.0 + self.vertices.len();

    self.is_in_face = false;
  }

  pub(crate) fn add_normals(&mut self, v: Normal) {
    if self.is_in_normals == false {
      self.is_in_normals = true;
      self.normals.clear();
      self.valid_nx.0 = self.valid_nx.1;
    }
    self.normals.push(v);
    self.valid_nx.1 = self.valid_nx.0 + self.normals.len();

    self.is_in_face = false;
  }

  pub(crate) fn add_tex_vertices(&mut self, v: TVertex) {
    if self.is_in_tex_vertices == false {
      self.is_in_tex_vertices = true;
      self.tex_vertices.clear();
      self.valid_tx.0 = self.valid_tx.1;
    }
    self.tex_vertices.push(v);
    self.valid_tx.1 = self.valid_tx.0 + self.tex_vertices.len();

    self.is_in_face = false;
  }

  pub(crate) fn add_face(&mut self, face: Vec<Shape>) {
    if self.is_in_face == false {
      self.is_in_face = true;
      self.is_in_vertices = false;
      self.is_in_vertices = false;
      self.is_in_normals = false;
      self.is_in_tex_vertices = false;

      self.geometry_s.push(Geometry {
        material_name: Some(std::mem::take(&mut self.material_name)),
        shapes: Vec::new(),
      });
    }
    self.geometry_s.last_mut().unwrap().shapes.extend(face);
  }
}
