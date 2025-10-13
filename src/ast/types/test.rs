use super::*;
use indoc::indoc;
use similar_asserts::assert_eq;

fn field_def(t: impl Into<FieldType>) -> FieldDef {
    FieldDef {
        span: Span::MISSING,
        ty: t.into(),
        repeated: None,
    }
}

fn struct1_body() -> StructBody {
    StructBody {
        span: Span::MISSING,
        fields: vec![
            field_def(BaseType::Word),
            FieldDef {
                span: Span::MISSING,
                ty: BaseType::Long.into(),
                repeated: Some(NumericLiteral::from(2)),
            },
            field_def(TypeName::without_span("other")),
        ],
    }
}

#[test]
fn parse_field_type() {
    assert_eq!(
        "w".parse::<FieldType>().unwrap(),
        FieldType::from(BaseType::Word),
    );
}

#[test]
fn parse_struct() {
    assert_eq!(
        indoc! {"
                type :foo = {
                    w,
                    l 2,
                    :other
                }
                "
        }
        .parse::<TypeDef>()
        .unwrap(),
        TypeDef {
            span: Span::MISSING,
            name: TypeName::without_span("foo"),
            body: TypeDefBody::Struct(struct1_body()),
            align: None,
        }
    )
}

#[test]
fn print_struct() {
    assert_eq!(
        format!(
            "{}",
            TypeDef {
                span: Span::MISSING,
                name: TypeName::without_span("hello"),
                align: None,
                body: TypeDefBody::Struct(struct1_body())
            }
        ),
        indoc! {"
            type :hello = {
                w,
                l 2,
                :other
            }
        "}
        .trim_end()
    )
}

fn union1_body() -> UnionBody {
    UnionBody {
        span: Span::MISSING,
        variants: vec![
            struct1_body(),
            StructBody {
                span: Span::MISSING,
                fields: vec![FieldDef {
                    span: Span::MISSING,
                    ty: BaseType::Single.into(),
                    repeated: None,
                }],
            },
        ],
    }
}

#[test]
fn parse_union() {
    assert_eq!(
        indoc! {"
                type :hello = align 4 {
                    {
                        w,
                        l 2,
                        :other
                    }
                    {
                        s
                    }
                }
            "}
        .parse::<TypeDef>()
        .unwrap(),
        TypeDef {
            span: Span::MISSING,
            name: TypeName::without_span("hello"),
            align: Some(AlignSpec {
                span: Span::MISSING,
                value: NumericLiteral::from(4)
            }),
            body: TypeDefBody::Union(union1_body()),
        }
    )
}

#[test]
fn print_union() {
    assert_eq!(
        format!(
            "{}",
            TypeDef {
                span: Span::MISSING,
                name: TypeName::without_span("hello"),
                align: None,
                body: TypeDefBody::Union(union1_body())
            }
        ),
        indoc! {"
            type :hello = {
                {
                    w,
                    l 2,
                    :other
                }
                {
                    s
                }
            }
        "}
        .trim_end()
    )
}

fn opaque1() -> TypeDef {
    TypeDef {
        span: Span::MISSING,
        name: TypeName::without_span("hello"),
        align: Some(AlignSpec {
            value: 8.into(),
            span: Span::MISSING,
        }),
        body: TypeDefBody::Opaque(OpaqueBody {
            span: Span::MISSING,
            size: 12.into(),
        }),
    }
}

#[test]
fn parse_opaque_type() {
    assert_eq!(
        "type :hello = align 8 { 12 }".parse::<TypeDef>().unwrap(),
        opaque1(),
    )
}

#[test]
fn print_opaque_type() {
    assert_eq!(format!("{}", opaque1()), "type :hello = align 8 { 12 }")
}
