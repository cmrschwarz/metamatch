use super::evaluate::Result;
use proc_macro::{
    Delimiter, Group, Punct, Spacing, Span, TokenStream, TokenTree,
};

pub struct TokenSink {
    pub data: Vec<TokenTree>,
    pub pending_raw: Vec<TokenTree>,
    pub force_raw_flush: bool,
    pub quote_for_rust: bool,
}

impl TokenSink {
    pub fn new(quote_for_rust: bool) -> Self {
        Self {
            data: Vec::new(),
            pending_raw: Vec::new(),
            quote_for_rust,
            force_raw_flush: false,
        }
    }

    pub fn push(&mut self, token: TokenTree) {
        self.flush_pending_raw();
        self.data.push(token);
    }

    pub fn push_raw(&mut self, token: TokenTree) {
        self.pending_raw.push(token);
    }

    pub fn extend_raw<I: IntoIterator<Item = TokenTree>>(
        &mut self,
        tokens: I,
    ) {
        self.pending_raw.extend(tokens);
    }

    pub fn extend_from_slice(&mut self, tokens: &[TokenTree]) {
        self.flush_pending_raw();
        self.data.extend_from_slice(tokens);
    }

    pub fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, tokens: I) {
        self.flush_pending_raw();
        self.data.extend(tokens);
    }

    pub fn append_group(
        &mut self,
        delim: Delimiter,
        span: Span,
        map_inner: impl FnOnce(&mut TokenSink) -> Result<()>,
    ) -> Result<()> {
        self.flush_pending_raw();
        let len_before = self.data.len();
        map_inner(self)?;
        self.flush_pending_raw();
        let mut group = Group::new(
            delim,
            TokenStream::from_iter(self.data.drain(len_before..)),
        );
        group.set_span(span);
        self.push(TokenTree::Group(group));
        Ok(())
    }

    pub fn append_raw(
        &mut self,
        mut map_inner: impl FnMut(&mut TokenSink) -> Result<()>,
    ) -> Result<()> {
        let len_before = self.data.len();
        let res = map_inner(self);
        self.pending_raw.extend(self.data.drain(len_before..));
        res
    }

    pub fn flush_pending_raw(&mut self) {
        if self.pending_raw.is_empty() && !self.force_raw_flush {
            return;
        }
        self.force_raw_flush = false;
        // Create a #(...) block containing all pending raw content
        self.data
            .push(TokenTree::Punct(Punct::new('#', Spacing::Alone)));

        let mut group = Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter(self.pending_raw.drain(..)),
        );
        group.set_span(Span::call_site());
        self.data.push(TokenTree::Group(group));
    }

    pub fn into_vec(mut self) -> Vec<TokenTree> {
        self.flush_pending_raw();
        self.data
    }

    pub fn take(&mut self) -> Vec<TokenTree> {
        self.flush_pending_raw();
        std::mem::take(&mut self.data)
    }

    pub fn take_sink(&mut self) -> TokenSink {
        TokenSink {
            data: self.take(),
            pending_raw: std::mem::take(&mut self.pending_raw),
            quote_for_rust: self.quote_for_rust,
            force_raw_flush: false,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty() && self.pending_raw.is_empty()
    }
}
