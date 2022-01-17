use std::fmt::Display;

pub fn pretty_print<T: Display>(vec: Vec<T>) -> String {
    let mut buffer = String::new();

    buffer += "[";
    for val in vec {
        buffer += &format!(" {},", val);
    }
    buffer.pop();
    buffer += " ]";

    buffer
}
