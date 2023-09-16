use proc_macro2::Span;
use syn::punctuated::Punctuated;
use syn::Token;

#[cfg(test)]
use {pretty_assertions::assert_eq as pretty_assert_eq, syn::parse_quote};

mod sealed {
    use crate::*;

    pub trait HasVis {
        fn visibility_mut(&mut self) -> &mut syn::Visibility;
    }

    pub trait HasAttrs {
        fn attrs_mut(&mut self) -> &mut Vec<syn::Attribute>;
    }

    macro_rules! has_visiblity {
        ($($ty:ty),* $(,)?) => {
            $(
                impl HasVis for $ty {
                    fn visibility_mut(&mut self) -> &mut syn::Visibility {
                        &mut self.vis
                    }
                }
            )*
        };
    }

    macro_rules! has_attrs {
        ($($ty:ty),* $(,)?) => {
            $(
                impl HasAttrs for $ty {
                    fn attrs_mut(&mut self) -> &mut Vec<syn::Attribute> {
                        &mut self.attrs
                    }
                }
            )*
        };
    }

    has_attrs!(
        field::NamedField,
        field::UnnamedField,
        strukt::StructBuilder,
        strukt::NamedStructBuilder,
        enom::EnumBuilder,
        syn::Variant,
    );
    has_visiblity!(
        field::NamedField,
        field::UnnamedField,
        strukt::StructBuilder,
        strukt::NamedStructBuilder,
        enom::EnumBuilder,
    );
}

pub trait ChangeVisibility: sealed::HasVis {
    fn public(mut self) -> Self
    where
        Self: Sized,
    {
        *self.visibility_mut() = syn::Visibility::Public(token::pub_());
        self
    }
}
impl<T> ChangeVisibility for T where T: sealed::HasVis {}

pub trait ChangeAttrs: sealed::HasAttrs {
    fn with_attr(mut self, attr: syn::Attribute) -> Self
    where
        Self: Sized,
    {
        self.attrs_mut().push(attr);
        self
    }
}
impl<T> ChangeAttrs for T where T: sealed::HasAttrs {}

pub trait BuildIdent {
    fn build_ident(self) -> syn::Ident;
}

impl BuildIdent for syn::Ident {
    fn build_ident(self) -> syn::Ident {
        self
    }
}

impl BuildIdent for &str {
    /// # Panics
    /// - if the input string is neither a keyword nor a legal variable name.
    fn build_ident(self) -> syn::Ident {
        syn::Ident::new(self, Span::call_site())
    }
}

pub trait BuildPathSegment {
    fn build_path_segment(self) -> syn::PathSegment;
}

impl BuildPathSegment for syn::PathSegment {
    fn build_path_segment(self) -> syn::PathSegment {
        self
    }
}

impl BuildPathSegment for &str {
    /// # Panics
    /// - if the input string is neither a keyword nor a legal variable name.
    fn build_path_segment(self) -> syn::PathSegment {
        syn::PathSegment {
            ident: self.build_ident(),
            arguments: syn::PathArguments::None,
        }
    }
}

pub trait BuildType {
    fn build_type(self) -> syn::Type;
}

impl BuildType for syn::Type {
    fn build_type(self) -> syn::Type {
        self
    }
}

impl BuildType for syn::Path {
    fn build_type(self) -> syn::Type {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: self,
        })
    }
}

pub mod field {
    use crate::*;

    pub fn named(name: impl BuildIdent, ty: impl BuildType) -> NamedField {
        NamedField {
            ident: name.build_ident(),
            ty: ty.build_type(),
            vis: vis::private(),
            attrs: vec![],
        }
    }

    #[derive(Clone)]
    pub struct NamedField {
        pub(crate) attrs: Vec<syn::Attribute>,
        pub(crate) vis: syn::Visibility,
        pub(crate) ident: syn::Ident,
        pub(crate) ty: syn::Type,
    }

    impl NamedField {
        pub fn build(self) -> syn::Field {
            let Self {
                attrs,
                vis,
                ident,
                ty,
            } = self;
            syn::Field {
                attrs,
                vis,
                mutability: syn::FieldMutability::None,
                ident: Some(ident),
                colon_token: Some(token::colon()),
                ty,
            }
        }
    }

    pub fn unnamed(ty: impl BuildType) -> UnnamedField {
        UnnamedField {
            ty: ty.build_type(),
            vis: vis::private(),
            attrs: vec![],
        }
    }

    #[derive(Clone)]
    pub struct UnnamedField {
        pub(crate) attrs: Vec<syn::Attribute>,
        pub(crate) vis: syn::Visibility,
        pub(crate) ty: syn::Type,
    }

    impl UnnamedField {
        pub fn build(self) -> syn::Field {
            let Self { attrs, vis, ty } = self;
            syn::Field {
                attrs,
                vis,
                mutability: syn::FieldMutability::None,
                ident: None,
                colon_token: None,
                ty,
            }
        }
    }
}

pub mod variant {
    use crate::*;

    pub fn unit(ident: impl BuildIdent) -> syn::Variant {
        syn::Variant {
            attrs: vec![],
            ident: ident.build_ident(),
            fields: syn::Fields::Unit,
            discriminant: None,
        }
    }

    pub fn tuple(ident: impl BuildIdent) -> TupleVariantBuilder {
        TupleVariantBuilder {
            ident: ident.build_ident(),
            fields: vec![],
            attrs: vec![],
        }
    }

    pub fn named(ident: impl BuildIdent) -> NamedVariantBuilder {
        NamedVariantBuilder {
            ident: ident.build_ident(),
            attrs: vec![],
            fields: vec![],
        }
    }

    #[derive(Clone)]
    pub struct TupleVariantBuilder {
        ident: syn::Ident,
        attrs: Vec<syn::Attribute>,
        fields: Vec<field::UnnamedField>,
    }

    impl TupleVariantBuilder {
        pub fn field(mut self, field: field::UnnamedField) -> Self {
            self.fields.push(field);
            self
        }
        pub fn fields(mut self, fields: impl IntoIterator<Item = field::UnnamedField>) -> Self {
            self.fields.extend(fields);
            self
        }
        pub fn build(self) -> syn::Variant {
            let Self {
                ident,
                attrs,
                fields,
            } = self;
            syn::Variant {
                attrs,
                ident,
                fields: syn::Fields::Unnamed(syn::FieldsUnnamed {
                    paren_token: token::paren(),
                    unnamed: fields
                        .into_iter()
                        .map(
                            |field::UnnamedField {
                                 attrs,
                                 vis: _ignored,
                                 ty,
                             }| syn::Field {
                                attrs,
                                vis: syn::Visibility::Inherited,
                                mutability: syn::FieldMutability::None,
                                ident: None,
                                colon_token: None,
                                ty,
                            },
                        )
                        .collect(),
                }),
                discriminant: None,
            }
        }
    }

    #[derive(Clone)]
    pub struct NamedVariantBuilder {
        ident: syn::Ident,
        attrs: Vec<syn::Attribute>,
        fields: Vec<field::NamedField>,
    }

    impl NamedVariantBuilder {
        pub fn field(mut self, field: field::NamedField) -> Self {
            self.fields.push(field);
            self
        }
        pub fn fields(mut self, fields: impl IntoIterator<Item = field::NamedField>) -> Self {
            self.fields.extend(fields);
            self
        }
        pub fn build(self) -> syn::Variant {
            let Self {
                ident,
                attrs,
                fields,
            } = self;
            syn::Variant {
                attrs,
                ident,
                fields: syn::Fields::Named(syn::FieldsNamed {
                    brace_token: token::brace(),
                    named: fields
                        .into_iter()
                        .map(
                            |field::NamedField {
                                 attrs,
                                 vis: _ignored,
                                 ident,
                                 ty,
                             }| syn::Field {
                                attrs,
                                vis: syn::Visibility::Inherited,
                                mutability: syn::FieldMutability::None,
                                ident: Some(ident),
                                colon_token: Some(token::colon()),
                                ty,
                            },
                        )
                        .collect(),
                }),
                discriminant: None,
            }
        }
    }
}

pub mod enom {
    use crate::*;

    pub fn new(ident: impl BuildIdent) -> EnumBuilder {
        EnumBuilder {
            ident: ident.build_ident(),
            variants: Punctuated::new(),
            attrs: vec![],
            vis: vis::private(),
        }
    }

    #[derive(Clone)]
    pub struct EnumBuilder {
        pub(crate) attrs: Vec<syn::Attribute>,
        pub(crate) vis: syn::Visibility,
        ident: syn::Ident,
        variants: Punctuated<syn::Variant, Token![,]>,
    }

    impl EnumBuilder {
        pub fn variant(mut self, variant: syn::Variant) -> Self {
            self.variants.push(variant);
            self
        }
        pub fn variants(mut self, variants: impl IntoIterator<Item = syn::Variant>) -> Self {
            self.variants.extend(variants);
            self
        }
        pub fn build(self) -> syn::DeriveInput {
            let Self {
                attrs,
                vis,
                ident,
                variants,
            } = self;
            syn::DeriveInput {
                attrs,
                vis,
                ident,
                generics: generics::none(),
                data: syn::Data::Enum(syn::DataEnum {
                    enum_token: token::enum_(),
                    brace_token: token::brace(),
                    variants,
                }),
            }
        }
    }
}

pub mod strukt {
    use crate::*;

    pub fn new(ident: impl BuildIdent) -> StructBuilder {
        StructBuilder {
            ident: ident.build_ident(),
            attrs: vec![],
            vis: vis::private(),
        }
    }

    #[derive(Clone)]
    pub struct StructBuilder {
        pub(crate) attrs: Vec<syn::Attribute>,
        pub(crate) vis: syn::Visibility,
        ident: syn::Ident,
    }

    impl StructBuilder {
        pub fn unit(self) -> syn::DeriveInput {
            let Self { vis, ident, attrs } = self;
            syn::DeriveInput {
                attrs,
                vis,
                ident,
                generics: generics::none(),
                data: syn::Data::Struct(syn::DataStruct {
                    struct_token: token::struct_(),
                    fields: syn::Fields::Unit,
                    semi_token: Some(token::semi()),
                }),
            }
        }
        pub fn named(self) -> NamedStructBuilder {
            let Self { attrs, vis, ident } = self;
            NamedStructBuilder {
                fields: vec![],
                attrs,
                vis,
                ident,
            }
        }
    }

    #[derive(Clone)]
    pub struct NamedStructBuilder {
        pub(crate) attrs: Vec<syn::Attribute>,
        pub(crate) vis: syn::Visibility,
        ident: syn::Ident,
        fields: Vec<field::NamedField>,
    }

    impl NamedStructBuilder {
        pub fn field(mut self, field: field::NamedField) -> Self {
            self.fields.push(field);
            self
        }
        pub fn fields(mut self, fields: impl IntoIterator<Item = field::NamedField>) -> Self {
            self.fields.extend(fields);
            self
        }
        pub fn build(self) -> syn::DeriveInput {
            let Self {
                fields,
                attrs,
                vis,
                ident,
            } = self;
            syn::DeriveInput {
                attrs,
                vis,
                ident,
                generics: generics::none(),
                data: syn::Data::Struct(syn::DataStruct {
                    struct_token: token::struct_(),
                    fields: syn::Fields::Named(syn::FieldsNamed {
                        brace_token: token::brace(),
                        named: fields.into_iter().map(field::NamedField::build).collect(),
                    }),
                    semi_token: None,
                }),
            }
        }
    }

    #[test]
    fn test() {
        let expected = parse_quote! {
            pub struct Yak {
                name: Option<String>,
                pub is_shaved: bool
            }
        };
        let actual = strukt::new("Yak")
            .public()
            .named()
            .fields([
                field::named(
                    "name",
                    path::new_with_angled_args("Option", [path::just("String")]).build(),
                ),
                field::named("is_shaved", path::just("bool")).public(),
            ])
            .build();
        pretty_assert_eq!(actual, expected);
    }
}

pub mod path {

    use crate::*;

    pub fn new(root: impl BuildPathSegment) -> PathBuilder {
        PathBuilder {
            leading_colon: None,
            segments: util::punctuated(root.build_path_segment()),
        }
    }

    pub fn new_with_angled_args(
        root: impl BuildIdent,
        args: impl IntoIterator<Item = syn::Path>,
    ) -> PathBuilder {
        PathBuilder {
            leading_colon: None,
            segments: util::punctuated(syn::PathSegment {
                ident: root.build_ident(),
                arguments: syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: token::lt(),
                        args: args
                            .into_iter()
                            .map(|path| {
                                syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                                    qself: None,
                                    path,
                                }))
                            })
                            .collect(),
                        gt_token: token::gt(),
                    },
                ),
            }),
        }
    }

    pub fn absolute(root: impl BuildPathSegment) -> PathBuilder {
        PathBuilder {
            leading_colon: Some(token::path_sep()),
            segments: util::punctuated(root.build_path_segment()),
        }
    }

    pub fn just(segment: impl BuildPathSegment) -> syn::Path {
        path::new(segment).build()
    }

    #[derive(Clone)]
    pub struct PathBuilder {
        leading_colon: Option<Token![::]>,
        segments: Punctuated<syn::PathSegment, Token![::]>,
    }

    impl PathBuilder {
        pub fn absolute(mut self) -> Self {
            self.leading_colon = Some(token::path_sep());
            self
        }
        pub fn then(mut self, next: impl BuildPathSegment) -> Self {
            self.segments.push(next.build_path_segment());
            self
        }
        pub fn then_angled_args(
            mut self,
            next: impl BuildIdent,
            args: impl IntoIterator<Item = syn::Path>,
        ) -> Self {
            self.segments.push(syn::PathSegment {
                ident: next.build_ident(),
                arguments: syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: token::lt(),
                        args: args
                            .into_iter()
                            .map(|path| {
                                syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                                    qself: None,
                                    path,
                                }))
                            })
                            .collect(),
                        gt_token: token::gt(),
                    },
                ),
            });
            self
        }
        pub fn build(self) -> syn::Path {
            let Self {
                leading_colon,
                segments,
            } = self;
            syn::Path {
                leading_colon,
                segments,
            }
        }
    }

    #[test]
    fn test() {
        let expected = parse_quote!(::core::cell::RefCell<T>);
        let actual = path::absolute("core")
            .then("cell")
            .then_angled_args("RefCell", [path::just("T")])
            .build();
        assert_eq!(actual, expected);
    }
}

pub mod vis {
    use crate::*;

    pub fn public() -> syn::Visibility {
        syn::Visibility::Public(token::pub_())
    }
    pub fn private() -> syn::Visibility {
        syn::Visibility::Inherited
    }
}

pub mod attr {
    use proc_macro2::{TokenStream, TokenTree};

    use crate::*;

    pub fn doc(s: &str) -> syn::Attribute {
        syn::Attribute {
            pound_token: token::pound(),
            style: syn::AttrStyle::Outer,
            bracket_token: token::bracket(),
            meta: syn::Meta::NameValue(syn::MetaNameValue {
                path: path::just("doc"),
                eq_token: token::eq(),
                value: syn::Expr::Lit(syn::ExprLit {
                    attrs: vec![],
                    lit: syn::Lit::Str(syn::LitStr::new(s, Span::call_site())),
                }),
            }),
        }
    }

    pub fn must_use() -> syn::Attribute {
        syn::Attribute {
            pound_token: token::pound(),
            style: syn::AttrStyle::Outer,
            bracket_token: token::bracket(),
            meta: syn::Meta::Path(path::just("must_use")),
        }
    }

    pub fn derive<T>(derives: impl IntoIterator<Item = T>) -> syn::Attribute
    where
        T: BuildIdent,
    {
        syn::Attribute {
            pound_token: token::pound(),
            style: syn::AttrStyle::Outer,
            bracket_token: token::bracket(),
            meta: syn::Meta::List(syn::MetaList {
                path: path::just("derive"),
                delimiter: syn::MacroDelimiter::Paren(token::paren()),
                tokens: TokenStream::from_iter(
                    derives
                        .into_iter()
                        .map(|it| TokenTree::Ident(it.build_ident())),
                ),
            }),
        }
    }
}

pub mod generics {
    use crate::*;

    pub fn none() -> syn::Generics {
        syn::Generics {
            lt_token: None,
            params: Punctuated::new(),
            gt_token: None,
            where_clause: None,
        }
    }
}

pub mod token {
    use crate::*;

    macro_rules! make {
        ($($fn:ident -> $ty:ident),* $(,)?) => {
            $(
                pub fn $fn() -> syn::token::$ty {
                    ::syn::token::$ty(Span::call_site())
                }
            )*
        };
    }

    make! {
        brace -> Brace,
        bracket -> Bracket,
        colon -> Colon,
        eq -> Eq,
        enum_ -> Enum,
        gt -> Gt,
        lt -> Lt,
        paren -> Paren,
        path_sep -> PathSep,
        pound -> Pound,
        pub_ -> Pub,
        semi -> Semi,
        struct_ -> Struct,
    }
}

mod util {
    use syn::punctuated::Punctuated;

    pub fn punctuated<T, P>(t: T) -> Punctuated<T, P>
    where
        P: Default,
    {
        Punctuated::from_iter([t])
    }
}
