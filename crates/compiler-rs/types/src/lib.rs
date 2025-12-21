//! SuperPascal Type System
//!
//! This crate defines the type system for SuperPascal, including type representation,
//! type operations (equality, compatibility, size calculation), and type checking.

// ast::Node not needed yet, will be used when converting AST types to Type

/// Type representation for SuperPascal
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Primitive types
    Primitive(PrimitiveType),
    /// Array type: array[index_type] of element_type (static array)
    Array {
        index_type: Box<Type>,
        element_type: Box<Type>,
        /// Size in bytes (calculated during semantic analysis)
        size: Option<usize>,
    },
    /// Dynamic array type: array of element_type (no fixed size)
    DynamicArray {
        element_type: Box<Type>,
    },
    /// Record type: record fields... end
    Record {
        fields: Vec<Field>,
        /// Size in bytes (calculated during semantic analysis)
        size: Option<usize>,
    },
    /// Pointer type: ^type
    Pointer {
        base_type: Box<Type>,
    },
    /// Named type (type alias)
    Named {
        name: String,
    },
    /// Error type (for error recovery)
    Error,
}

/// Primitive types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Integer,  // 16-bit signed integer
    Byte,     // 8-bit unsigned integer
    Word,     // 16-bit unsigned integer
    Boolean,  // Boolean (1 byte)
    Char,     // Character (1 byte)
}

impl PrimitiveType {
    /// Get the size in bytes for a primitive type
    pub fn size(&self) -> usize {
        match self {
            PrimitiveType::Integer => 2,
            PrimitiveType::Byte => 1,
            PrimitiveType::Word => 2,
            PrimitiveType::Boolean => 1,
            PrimitiveType::Char => 1,
        }
    }

    /// Get the alignment requirement in bytes
    pub fn alignment(&self) -> usize {
        match self {
            PrimitiveType::Integer => 2,
            PrimitiveType::Byte => 1,
            PrimitiveType::Word => 2,
            PrimitiveType::Boolean => 1,
            PrimitiveType::Char => 1,
        }
    }
}

/// Record field
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    /// Field name
    pub name: String,
    /// Field type
    pub field_type: Box<Type>,
    /// Offset in bytes from start of record (calculated during semantic analysis)
    pub offset: Option<usize>,
}

impl Type {
    /// Create a primitive type
    pub fn primitive(prim: PrimitiveType) -> Self {
        Type::Primitive(prim)
    }

    /// Create an integer type
    pub fn integer() -> Self {
        Type::Primitive(PrimitiveType::Integer)
    }

    /// Create a byte type
    pub fn byte() -> Self {
        Type::Primitive(PrimitiveType::Byte)
    }

    /// Create a word type
    pub fn word() -> Self {
        Type::Primitive(PrimitiveType::Word)
    }

    /// Create a boolean type
    pub fn boolean() -> Self {
        Type::Primitive(PrimitiveType::Boolean)
    }

    /// Create a char type
    pub fn char() -> Self {
        Type::Primitive(PrimitiveType::Char)
    }

    /// Create an array type (static array)
    pub fn array(index_type: Type, element_type: Type) -> Self {
        Type::Array {
            index_type: Box::new(index_type),
            element_type: Box::new(element_type),
            size: None,
        }
    }

    /// Create a dynamic array type
    pub fn dynamic_array(element_type: Type) -> Self {
        Type::DynamicArray {
            element_type: Box::new(element_type),
        }
    }

    /// Create a record type
    pub fn record(fields: Vec<Field>) -> Self {
        Type::Record {
            fields,
            size: None,
        }
    }

    /// Create a pointer type
    pub fn pointer(base_type: Type) -> Self {
        Type::Pointer {
            base_type: Box::new(base_type),
        }
    }

    /// Create a named type
    pub fn named(name: String) -> Self {
        Type::Named { name }
    }

    /// Check if two types are equal (structural equality)
    pub fn equals(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (
                Type::Array {
                    index_type: idx1,
                    element_type: elem1,
                    ..
                },
                Type::Array {
                    index_type: idx2,
                    element_type: elem2,
                    ..
                },
            ) => idx1.equals(idx2) && elem1.equals(elem2),
            (
                Type::DynamicArray {
                    element_type: elem1,
                },
                Type::DynamicArray {
                    element_type: elem2,
                },
            ) => elem1.equals(elem2),
            (Type::Record { fields: f1, .. }, Type::Record { fields: f2, .. }) => {
                if f1.len() != f2.len() {
                    return false;
                }
                f1.iter()
                    .zip(f2.iter())
                    .all(|(field1, field2)| {
                        field1.name == field2.name && field1.field_type.equals(&field2.field_type)
                    })
            }
            (
                Type::Pointer { base_type: b1 },
                Type::Pointer { base_type: b2 },
            ) => b1.equals(b2),
            (Type::Named { name: n1 }, Type::Named { name: n2 }) => n1 == n2,
            (Type::Error, Type::Error) => true,
            _ => false,
        }
    }

    /// Check if a type is assignment-compatible with another type
    /// In Pascal, assignment compatibility is more lenient than equality
    pub fn is_assignable_to(&self, target: &Type) -> bool {
        // Exact match
        if self.equals(target) {
            return true;
        }

        // Integer/Byte/Word compatibility (Tier 1: simple rules)
        match (self, target) {
            // Integer can be assigned to Integer
            (Type::Primitive(PrimitiveType::Integer), Type::Primitive(PrimitiveType::Integer)) => {
                true
            }
            // Byte can be assigned to Integer or Word
            (
                Type::Primitive(PrimitiveType::Byte),
                Type::Primitive(PrimitiveType::Integer) | Type::Primitive(PrimitiveType::Word),
            ) => true,
            // Word can be assigned to Integer (with potential overflow warning)
            (
                Type::Primitive(PrimitiveType::Word),
                Type::Primitive(PrimitiveType::Integer),
            ) => true,
            // Char can be assigned to Byte
            (
                Type::Primitive(PrimitiveType::Char),
                Type::Primitive(PrimitiveType::Byte),
            ) => true,
            // Boolean is only compatible with Boolean
            (Type::Primitive(PrimitiveType::Boolean), Type::Primitive(PrimitiveType::Boolean)) => {
                true
            }
            // Error type is compatible with everything (error recovery)
            (Type::Error, _) | (_, Type::Error) => true,
            _ => false,
        }
    }

    /// Calculate the size of a type in bytes
    /// Returns None if size cannot be determined (e.g., open arrays, incomplete types)
    pub fn size(&self) -> Option<usize> {
        match self {
            Type::Primitive(prim) => Some(prim.size()),
            Type::Array { size, .. } => *size,
            Type::DynamicArray { .. } => None, // Dynamic arrays have no fixed size
            Type::Record { size, .. } => *size,
            Type::Pointer { .. } => Some(2), // Pointers are 16-bit (2 bytes) on 8-bit/16-bit targets
            Type::Named { .. } => None, // Need to resolve named type first
            Type::Error => None,
        }
    }

    /// Calculate the alignment requirement in bytes
    pub fn alignment(&self) -> usize {
        match self {
            Type::Primitive(prim) => prim.alignment(),
            Type::Array { element_type, .. } => element_type.alignment(),
            Type::DynamicArray { element_type } => element_type.alignment(),
            Type::Record { fields, .. } => {
                // Record alignment is the maximum alignment of its fields
                fields
                    .iter()
                    .map(|f| f.field_type.alignment())
                    .max()
                    .unwrap_or(1)
            }
            Type::Pointer { .. } => 2, // Pointers are 16-bit aligned
            Type::Named { .. } => 1, // Unknown, use minimum
            Type::Error => 1,
        }
    }

    /// Calculate record field offsets
    /// This should be called during semantic analysis after all fields are known
    pub fn calculate_record_offsets(&mut self) {
        if let Type::Record { fields, size } = self {
            let mut offset = 0;
            for field in fields.iter_mut() {
                // Align offset to field's alignment requirement
                let align = field.field_type.alignment();
                offset = (offset + align - 1) / align * align;
                field.offset = Some(offset);
                offset += field.field_type.size().unwrap_or(0);
            }
            // Align total size to record's alignment
            // Calculate alignment from fields (max of field alignments)
            let record_align = fields
                .iter()
                .map(|f| f.field_type.alignment())
                .max()
                .unwrap_or(1);
            let total_size = (offset + record_align - 1) / record_align * record_align;
            *size = Some(total_size);
        }
    }

    /// Calculate array size
    /// For now, we assume fixed-size arrays (Tier 1)
    /// Returns None if size cannot be determined
    pub fn calculate_array_size(&mut self) -> Option<usize> {
        if let Type::Array {
            index_type: _,
            element_type: _,
            size,
        } = self
        {
            // For Tier 1, we assume arrays are indexed by integer/byte/word
            // Size calculation requires knowing the array bounds, which we'll add later
            // For now, return None (will be calculated during semantic analysis)
            *size
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===== Primitive Type Tests =====

    #[test]
    fn test_primitive_type_sizes() {
        assert_eq!(PrimitiveType::Integer.size(), 2);
        assert_eq!(PrimitiveType::Byte.size(), 1);
        assert_eq!(PrimitiveType::Word.size(), 2);
        assert_eq!(PrimitiveType::Boolean.size(), 1);
        assert_eq!(PrimitiveType::Char.size(), 1);
    }

    #[test]
    fn test_primitive_type_alignments() {
        assert_eq!(PrimitiveType::Integer.alignment(), 2);
        assert_eq!(PrimitiveType::Byte.alignment(), 1);
        assert_eq!(PrimitiveType::Word.alignment(), 2);
        assert_eq!(PrimitiveType::Boolean.alignment(), 1);
        assert_eq!(PrimitiveType::Char.alignment(), 1);
    }

    #[test]
    fn test_type_creation() {
        assert_eq!(Type::integer(), Type::Primitive(PrimitiveType::Integer));
        assert_eq!(Type::byte(), Type::Primitive(PrimitiveType::Byte));
        assert_eq!(Type::word(), Type::Primitive(PrimitiveType::Word));
        assert_eq!(Type::boolean(), Type::Primitive(PrimitiveType::Boolean));
        assert_eq!(Type::char(), Type::Primitive(PrimitiveType::Char));
    }

    // ===== Type Equality Tests =====

    #[test]
    fn test_type_equality_primitive() {
        assert!(Type::integer().equals(&Type::integer()));
        assert!(Type::byte().equals(&Type::byte()));
        assert!(!Type::integer().equals(&Type::byte()));
    }

    #[test]
    fn test_type_equality_array() {
        let arr1 = Type::array(Type::integer(), Type::char());
        let arr2 = Type::array(Type::integer(), Type::char());
        let arr3 = Type::array(Type::byte(), Type::char());

        assert!(arr1.equals(&arr2));
        assert!(!arr1.equals(&arr3));
    }

    #[test]
    fn test_type_equality_record() {
        let rec1 = Type::record(vec![
            Field {
                name: "x".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
            Field {
                name: "y".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);

        let rec2 = Type::record(vec![
            Field {
                name: "x".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
            Field {
                name: "y".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);

        let rec3 = Type::record(vec![Field {
            name: "x".to_string(),
            field_type: Box::new(Type::integer()),
            offset: None,
        }]);

        assert!(rec1.equals(&rec2));
        assert!(!rec1.equals(&rec3));
    }

    #[test]
    fn test_type_equality_pointer() {
        let ptr1 = Type::pointer(Type::integer());
        let ptr2 = Type::pointer(Type::integer());
        let ptr3 = Type::pointer(Type::byte());

        assert!(ptr1.equals(&ptr2));
        assert!(!ptr1.equals(&ptr3));
    }

    // ===== Assignment Compatibility Tests =====

    #[test]
    fn test_assignment_compatibility_exact_match() {
        assert!(Type::integer().is_assignable_to(&Type::integer()));
        assert!(Type::byte().is_assignable_to(&Type::byte()));
    }

    #[test]
    fn test_assignment_compatibility_byte_to_integer() {
        assert!(Type::byte().is_assignable_to(&Type::integer()));
        assert!(Type::byte().is_assignable_to(&Type::word()));
    }

    #[test]
    fn test_assignment_compatibility_word_to_integer() {
        assert!(Type::word().is_assignable_to(&Type::integer()));
    }

    #[test]
    fn test_assignment_compatibility_char_to_byte() {
        assert!(Type::char().is_assignable_to(&Type::byte()));
    }

    #[test]
    fn test_assignment_compatibility_boolean() {
        assert!(Type::boolean().is_assignable_to(&Type::boolean()));
        assert!(!Type::boolean().is_assignable_to(&Type::integer()));
    }

    #[test]
    fn test_assignment_compatibility_error_type() {
        assert!(Type::Error.is_assignable_to(&Type::integer()));
        assert!(Type::integer().is_assignable_to(&Type::Error));
    }

    // ===== Size Calculation Tests =====

    #[test]
    fn test_type_size_primitive() {
        assert_eq!(Type::integer().size(), Some(2));
        assert_eq!(Type::byte().size(), Some(1));
        assert_eq!(Type::word().size(), Some(2));
        assert_eq!(Type::boolean().size(), Some(1));
        assert_eq!(Type::char().size(), Some(1));
    }

    #[test]
    fn test_type_size_pointer() {
        let ptr = Type::pointer(Type::integer());
        assert_eq!(ptr.size(), Some(2)); // 16-bit pointer
    }

    #[test]
    fn test_type_size_named() {
        let named = Type::named("MyInt".to_string());
        assert_eq!(named.size(), None); // Need to resolve first
    }

    // ===== Alignment Tests =====

    #[test]
    fn test_type_alignment_primitive() {
        assert_eq!(Type::integer().alignment(), 2);
        assert_eq!(Type::byte().alignment(), 1);
        assert_eq!(Type::word().alignment(), 2);
    }

    #[test]
    fn test_type_alignment_array() {
        let arr = Type::array(Type::integer(), Type::char());
        assert_eq!(arr.alignment(), 1); // Element type (char) alignment
    }

    #[test]
    fn test_type_alignment_pointer() {
        let ptr = Type::pointer(Type::integer());
        assert_eq!(ptr.alignment(), 2);
    }

    // ===== Record Field Offset Tests =====

    #[test]
    fn test_record_field_offsets() {
        let mut rec = Type::record(vec![
            Field {
                name: "x".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
            Field {
                name: "y".to_string(),
                field_type: Box::new(Type::byte()),
                offset: None,
            },
        ]);

        rec.calculate_record_offsets();

        if let Type::Record { fields, size } = rec {
            assert_eq!(fields[0].offset, Some(0)); // x at offset 0
            assert_eq!(fields[1].offset, Some(2)); // y at offset 2 (aligned after integer)
            // Total size: 2 (integer) + 1 (byte) = 3, aligned to record alignment (2) = 4
            assert_eq!(size, Some(4));
        }
    }

    #[test]
    fn test_record_field_offsets_alignment() {
        let mut rec = Type::record(vec![
            Field {
                name: "a".to_string(),
                field_type: Box::new(Type::byte()),
                offset: None,
            },
            Field {
                name: "b".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);

        rec.calculate_record_offsets();

        if let Type::Record { fields, size } = rec {
            assert_eq!(fields[0].offset, Some(0)); // a at offset 0
            assert_eq!(fields[1].offset, Some(2)); // b at offset 2 (aligned to 2)
            assert_eq!(size, Some(4)); // Total: 2 (byte + padding) + 2 (integer) = 4
        }
    }

    // ===== Additional Assignment Compatibility Tests =====

    #[test]
    fn test_assignment_compatibility_array() {
        let arr1 = Type::array(Type::integer(), Type::char());
        let arr2 = Type::array(Type::integer(), Type::char());
        let arr3 = Type::array(Type::byte(), Type::char());

        // Arrays must match exactly
        assert!(arr1.equals(&arr2));
        assert!(!arr1.equals(&arr3));
        // Arrays are assignable if they're equal
        assert!(arr1.is_assignable_to(&arr2));
    }

    #[test]
    fn test_assignment_compatibility_record() {
        let rec1 = Type::record(vec![
            Field {
                name: "x".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);
        let rec2 = Type::record(vec![
            Field {
                name: "x".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);

        // Records must match exactly
        assert!(rec1.equals(&rec2));
        assert!(rec1.is_assignable_to(&rec2));
    }

    #[test]
    fn test_assignment_compatibility_pointer() {
        let ptr1 = Type::pointer(Type::integer());
        let ptr2 = Type::pointer(Type::integer());
        let ptr3 = Type::pointer(Type::byte());

        assert!(ptr1.equals(&ptr2));
        assert!(ptr1.is_assignable_to(&ptr2));
        assert!(!ptr1.equals(&ptr3));
        assert!(!ptr1.is_assignable_to(&ptr3));
    }

    #[test]
    fn test_assignment_compatibility_named_types() {
        let named1 = Type::named("MyInt".to_string());
        let named2 = Type::named("MyInt".to_string());
        let named3 = Type::named("OtherInt".to_string());

        // Named types compare by name
        assert!(named1.equals(&named2));
        assert!(!named1.equals(&named3));
        // Assignment compatibility for named types requires resolution
        assert!(named1.is_assignable_to(&named2));
    }

    #[test]
    fn test_assignment_compatibility_incompatible_types() {
        // Test various incompatible combinations
        assert!(!Type::integer().is_assignable_to(&Type::boolean()));
        assert!(!Type::boolean().is_assignable_to(&Type::integer()));
        assert!(!Type::char().is_assignable_to(&Type::integer()));
        assert!(!Type::integer().is_assignable_to(&Type::char()));
    }

    // ===== Complex Type Tests =====

    #[test]
    fn test_nested_array_types() {
        // Array of arrays
        let inner = Type::array(Type::integer(), Type::char());
        let outer = Type::array(Type::integer(), inner.clone());

        assert_eq!(outer.alignment(), 1); // Element alignment
        // Size would be calculated during semantic analysis
    }

    #[test]
    fn test_record_with_array_field() {
        let mut rec = Type::record(vec![
            Field {
                name: "arr".to_string(),
                field_type: Box::new(Type::array(Type::integer(), Type::char())),
                offset: None,
            },
            Field {
                name: "count".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);

        rec.calculate_record_offsets();
        if let Type::Record { fields, size } = rec {
            assert_eq!(fields[0].offset, Some(0)); // arr at offset 0
            // count offset depends on array size (would be calculated)
            assert!(size.is_some());
        }
    }

    #[test]
    fn test_record_with_pointer_field() {
        let mut rec = Type::record(vec![
            Field {
                name: "ptr".to_string(),
                field_type: Box::new(Type::pointer(Type::integer())),
                offset: None,
            },
            Field {
                name: "value".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);

        rec.calculate_record_offsets();
        if let Type::Record { fields, size } = rec {
            assert_eq!(fields[0].offset, Some(0)); // ptr at offset 0 (2 bytes)
            assert_eq!(fields[1].offset, Some(2)); // value at offset 2 (aligned)
            assert_eq!(size, Some(4)); // Total: 2 (ptr) + 2 (integer) = 4
        }
    }

    #[test]
    fn test_complex_record_structure() {
        let mut rec = Type::record(vec![
            Field {
                name: "a".to_string(),
                field_type: Box::new(Type::byte()),
                offset: None,
            },
            Field {
                name: "b".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
            Field {
                name: "c".to_string(),
                field_type: Box::new(Type::byte()),
                offset: None,
            },
            Field {
                name: "d".to_string(),
                field_type: Box::new(Type::word()),
                offset: None,
            },
        ]);

        rec.calculate_record_offsets();
        if let Type::Record { fields, size } = rec {
            assert_eq!(fields[0].offset, Some(0)); // a at 0
            assert_eq!(fields[1].offset, Some(2)); // b at 2 (aligned)
            assert_eq!(fields[2].offset, Some(4)); // c at 4
            assert_eq!(fields[3].offset, Some(6)); // d at 6 (aligned to 2)
            assert_eq!(size, Some(8)); // Total aligned to 2
        }
    }

    #[test]
    fn test_empty_record() {
        let mut rec = Type::record(vec![]);
        rec.calculate_record_offsets();
        if let Type::Record { size, .. } = rec {
            assert_eq!(size, Some(0));
        }
    }

    #[test]
    fn test_record_single_field() {
        let mut rec = Type::record(vec![Field {
            name: "x".to_string(),
            field_type: Box::new(Type::integer()),
            offset: None,
        }]);

        rec.calculate_record_offsets();
        if let Type::Record { fields, size } = rec {
            assert_eq!(fields[0].offset, Some(0));
            assert_eq!(size, Some(2)); // Aligned to integer alignment (2)
        }
    }

    // ===== Type Size Edge Cases =====

    #[test]
    fn test_type_size_array_unknown() {
        let mut arr = Type::array(Type::integer(), Type::char());
        // Array size is None until calculated during semantic analysis
        assert_eq!(arr.size(), None);
        arr.calculate_array_size();
        // Still None without bounds information
        assert_eq!(arr.size(), None);
    }

    #[test]
    fn test_type_size_record_unknown() {
        let rec = Type::record(vec![Field {
            name: "x".to_string(),
            field_type: Box::new(Type::integer()),
            offset: None,
        }]);
        // Size is None until calculate_record_offsets is called
        assert_eq!(rec.size(), None);
    }

    #[test]
    fn test_type_size_error_type() {
        assert_eq!(Type::Error.size(), None);
    }

    // ===== Type Alignment Edge Cases =====

    #[test]
    fn test_type_alignment_record_empty() {
        let rec = Type::record(vec![]);
        assert_eq!(rec.alignment(), 1); // Minimum alignment
    }

    #[test]
    fn test_type_alignment_record_single_field() {
        let rec = Type::record(vec![Field {
            name: "x".to_string(),
            field_type: Box::new(Type::integer()),
            offset: None,
        }]);
        assert_eq!(rec.alignment(), 2); // Integer alignment
    }

    #[test]
    fn test_type_alignment_record_mixed() {
        let rec = Type::record(vec![
            Field {
                name: "a".to_string(),
                field_type: Box::new(Type::byte()),
                offset: None,
            },
            Field {
                name: "b".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);
        assert_eq!(rec.alignment(), 2); // Max of field alignments
    }

    #[test]
    fn test_type_alignment_named() {
        let named = Type::named("MyType".to_string());
        assert_eq!(named.alignment(), 1); // Unknown, use minimum
    }

    #[test]
    fn test_type_alignment_error() {
        assert_eq!(Type::Error.alignment(), 1);
    }

    // ===== Type Equality Edge Cases =====

    #[test]
    fn test_type_equality_named_types() {
        let named1 = Type::named("MyInt".to_string());
        let named2 = Type::named("MyInt".to_string());
        let named3 = Type::named("Other".to_string());

        assert!(named1.equals(&named2));
        assert!(!named1.equals(&named3));
    }

    #[test]
    fn test_type_equality_error_type() {
        assert!(Type::Error.equals(&Type::Error));
        assert!(!Type::Error.equals(&Type::integer()));
        assert!(!Type::integer().equals(&Type::Error));
    }

    #[test]
    fn test_type_equality_record_field_order() {
        let rec1 = Type::record(vec![
            Field {
                name: "x".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
            Field {
                name: "y".to_string(),
                field_type: Box::new(Type::byte()),
                offset: None,
            },
        ]);

        let rec2 = Type::record(vec![
            Field {
                name: "y".to_string(),
                field_type: Box::new(Type::byte()),
                offset: None,
            },
            Field {
                name: "x".to_string(),
                field_type: Box::new(Type::integer()),
                offset: None,
            },
        ]);

        // Records compare field-by-field in order
        assert!(!rec1.equals(&rec2)); // Different order
    }

    #[test]
    fn test_type_equality_record_field_names() {
        let rec1 = Type::record(vec![Field {
            name: "x".to_string(),
            field_type: Box::new(Type::integer()),
            offset: None,
        }]);

        let rec2 = Type::record(vec![Field {
            name: "y".to_string(),
            field_type: Box::new(Type::integer()),
            offset: None,
        }]);

        assert!(!rec1.equals(&rec2)); // Different field names
    }

    // ===== Type Helper Method Tests =====

    #[test]
    fn test_type_primitive_helper() {
        assert_eq!(
            Type::primitive(PrimitiveType::Integer),
            Type::Primitive(PrimitiveType::Integer)
        );
        assert_eq!(
            Type::primitive(PrimitiveType::Byte),
            Type::Primitive(PrimitiveType::Byte)
        );
    }

    #[test]
    fn test_type_array_helper() {
        let arr = Type::array(Type::integer(), Type::char());
        match arr {
            Type::Array {
                index_type,
                element_type,
                ..
            } => {
                assert!(index_type.equals(&Type::integer()));
                assert!(element_type.equals(&Type::char()));
            }
            _ => panic!("Expected Array type"),
        }
    }

    #[test]
    fn test_type_record_helper() {
        let rec = Type::record(vec![Field {
            name: "x".to_string(),
            field_type: Box::new(Type::integer()),
            offset: None,
        }]);
        match rec {
            Type::Record { fields, .. } => {
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].name, "x");
            }
            _ => panic!("Expected Record type"),
        }
    }

    #[test]
    fn test_type_pointer_helper() {
        let ptr = Type::pointer(Type::integer());
        match ptr {
            Type::Pointer { base_type } => {
                assert!(base_type.equals(&Type::integer()));
            }
            _ => panic!("Expected Pointer type"),
        }
    }

    #[test]
    fn test_type_named_helper() {
        let named = Type::named("MyInt".to_string());
        match named {
            Type::Named { name } => {
                assert_eq!(name, "MyInt");
            }
            _ => panic!("Expected Named type"),
        }
    }
}
