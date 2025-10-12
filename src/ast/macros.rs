macro_rules! impl_string_like {
    ($target:ident) => {
        impl Debug for $target {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.debug_tuple(stringify!($target))
                    .field(&self.text())
                    .finish()
            }
        }
        impl AsRef<str> for $target {
            #[inline]
            fn as_ref(&self) -> &str {
                self.text()
            }
        }
        impl Borrow<str> for $target {
            #[inline]
            fn borrow(&self) -> &str {
                self.text()
            }
        }
        impl equivalent::Equivalent<String> for $target {
            fn equivalent(&self, other: &String) -> bool {
                self.text().equivalent(other)
            }
        }
        impl equivalent::Comparable<String> for $target {
            fn compare(&self, key: &String) -> core::cmp::Ordering {
                self.text().cmp(key)
            }
        }
    };
}
macro_rules! impl_ident_like {
    ($target:ident) => {
        impl_string_like!($target);
        impl AsRef<Ident> for $target {
            #[inline]
            fn as_ref(&self) -> &Ident {
                self.ident()
            }
        }
        impl Borrow<Ident> for $target {
            #[inline]
            fn borrow(&self) -> &Ident {
                self.ident()
            }
        }
        impl equivalent::Equivalent<Ident> for $target {
            fn equivalent(&self, other: &Ident) -> bool {
                self.ident().equivalent(other)
            }
        }
        impl equivalent::Comparable<Ident> for $target {
            fn compare(&self, key: &Ident) -> core::cmp::Ordering {
                self.ident().cmp(key)
            }
        }
    };
}
