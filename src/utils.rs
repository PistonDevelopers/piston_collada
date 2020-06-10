use std::str::FromStr;
use xml::{Element};
use xml::Xml::{ElementNode, CharacterNode};

pub fn parse_string_to_vector<T: FromStr>(string: &str) -> Vec<T> {
    string.trim()
        .split(&[' ', '\n'][..])
        .map(|s| s.parse().ok().expect("Error parsing array in COLLADA file"))
        .collect()
}

pub fn get_array_content<T: FromStr>(element: &Element) -> Option<Vec<T>> {
    match element.children[0] {
        CharacterNode(ref contents) => Some(parse_string_to_vector(contents)),
        _ => None,
    }
}

pub fn has_attribute_with_value(e: &Element, name: &str, value: &str) -> bool {
    if let Some(s) = e.get_attribute(name, None) {
        s == value
    } else {
        false
    }
}

pub fn to_matrix_array(float_array: Vec<f32>) -> Vec<[[f32; 4]; 4]> {
    float_array.chunks(16).map(|chunk| {
        let mut matrix = [[0f32; 4]; 4];
        for (&chunk_value, matrix_value) in chunk.iter().zip(matrix.iter_mut().flat_map(|n| n.iter_mut())) {
            *matrix_value = chunk_value;
        }
        matrix
    }).collect()
}

///
/// Returns an iterator over all ElementNodes in an XML Element subtree with the given root,
/// using a pre-order tree traversal (root before children)
///
pub fn pre_order_iter<'a>(root: &'a Element) -> PreOrderIterator<'a> {
    PreOrderIterator { stack: vec![root] }
}

///
/// Returns an iterator over all ElementNodes in an XML Element subtree with the given root,
/// with their depth relative to the subtree root,
/// using a pre-order tree traversal (root before children)
///
pub fn pre_order_with_depth_iter<'a>(root: &'a Element) -> PreOrderWithDepthIterator<'a> {
    PreOrderWithDepthIterator { stack: vec![(root, 0)] }
}

pub struct PreOrderIterator<'a> {
    stack: Vec<&'a Element>
}

impl<'a> Iterator for PreOrderIterator<'a> {
    type Item = &'a Element;
    fn next(&mut self) -> Option<&'a Element> {
        let current_element = self.stack.pop();
        match current_element {
            Some(element) => {
                for child in element.children.iter().rev() {
                    if let ElementNode(ref e) = *child {
                        self.stack.push(e);
                    }
                }
            }
            None => ()
        }
        current_element
    }
}

pub struct PreOrderWithDepthIterator<'a> {
    stack: Vec<(&'a Element, usize)>
}

impl<'a> Iterator for PreOrderWithDepthIterator<'a> {
    type Item = (&'a Element, usize);
    fn next(&mut self) -> Option<(&'a Element, usize)> {
        match self.stack.pop() {
            Some((element, depth)) => {
                for child in element.children.iter().rev() {
                    if let ElementNode(ref e) = *child {
                        self.stack.push((e, depth + 1));
                    }
                }
                Some((element, depth))
            }
            None => None
        }
    }
}
