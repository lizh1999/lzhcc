#include "lzhcc_lex.h"
#include <cassert>
#include <filesystem>

namespace fs = std::filesystem;

namespace lzhcc {

TokenCursor::TokenCursor(CharCursorFn cursor, Context *context)
    : sb_define(context->push_identifier("define")),
      sb_undef(context->push_identifier("undef")),
      sb_include(context->push_identifier("include")),
      sb_if(context->push_identifier("if")),
      sb_ifdef(context->push_identifier("ifdef")),
      sb_ifndef(context->push_identifier("ifndef")),
      sb_else(context->push_identifier("else")),
      sb_elif(context->push_identifier("elif")),
      sb_endif(context->push_identifier("endif")), top_cursor_(cursor, context),
      context_(context) {
  top_token_ = top_cursor_();
}

auto TokenCursor::text_fn() -> std::function<Token()> {
  return std::bind(&TokenCursor::text, this);
}

auto TokenCursor::text() -> Token {
  if (top_token_.kind == TokenKind::eof) {
    top_token_.start_of_line = true;
    return top_token_;
  }
  auto token = top_token_;
  advance_top_token();
  if (!token.start_of_line || token.kind != TokenKind::hash) {
    return token;
  }
  if (top_token_.start_of_line) {
    return text();
  } else {
    auto token = top_token_;
    advance_top_token();
    if (token.kind != TokenKind::identifier) {
      context_->fatal(token.location, "");
    }
    auto directive = token.inner;
    if (directive == sb_include) {
      include_file();
      return text();
    }
    if (directive == sb_define) {
      define_macro();
      return text();
    }
    if (directive == sb_undef) {
      remove_macro();
      return text();
    }
    if (directive == sb_if) {
      cond_stack_.push(token.location);
      handle_if();
      return text();
    }
    if (directive == sb_ifdef) {
      cond_stack_.push(token.location);
      handle_ifdef();
      return text();
    }
    if (directive == sb_ifndef) {
      cond_stack_.push(token.location);
      handle_ifndef();
      return text();
    }
    if (directive == sb_else || directive == sb_elif) {
      if (cond_stack_.empty()) {
        context_->fatal(token.location, "");
      }
      cond_stack_.pop();
      skip_cond();
      return text();
    }
    if (directive == sb_endif) {
      if (cond_stack_.empty()) {
        context_->fatal(token.location, "");
      }
      cond_stack_.pop();
      return text();
    }
    assert(false);
  }
}

auto TokenCursor::advance_top_token() -> void {
  assert(top_token_.kind != TokenKind::eof);
  top_token_ = top_cursor_();
  while (top_token_.kind == TokenKind::eof && !token_stack_.empty()) {
    top_token_ = token_stack_.top();
    top_cursor_ = cursor_stack_.top();
    token_stack_.pop();
    cursor_stack_.pop();
  }
}

auto TokenCursor::skip_line() -> void {
  if (top_token_.start_of_line) {
    return;
  }
  while (!top_token_.start_of_line) {
    advance_top_token();
  }
}

auto TokenCursor::skip_cond() -> void {
  int depth = 1;
  while (top_token_.kind != TokenKind::eof) {
    if (!top_token_.start_of_line || top_token_.kind != TokenKind::hash) {
      advance_top_token();
      continue;
    }
    advance_top_token();
    if (top_token_.kind != TokenKind::identifier) {
      continue;
    }
    if (top_token_.inner == sb_if) {
      depth++;
    }
    if (top_token_.inner == sb_endif) {
      advance_top_token();
      if (--depth == 0) {
        break;
      }
    }
  }
}

auto TokenCursor::include_file() -> void {
  auto token = top_token_;
  advance_top_token();
  if (token.kind != TokenKind::string) {
    context_->fatal(token.location, "");
  }
  fs::path path = context_->filename(token.location);
  auto name = context_->storage(token.inner);
  skip_line();
  name.remove_prefix(1);
  name.remove_suffix(1);
  path = path.parent_path() / name;
  if (!fs::exists(path)) {
    context_->fatal(token.location, "");
  }
  cursor_stack_.push(std::move(top_cursor_));
  token_stack_.push(top_token_);
  auto chars = context_->append_file(path);
  SourceCursor cursor(std::move(chars), context_);

  top_cursor_ = std::move(cursor);
  top_token_ = top_cursor_();
  if (top_token_.kind == TokenKind::eof) {
    top_token_ = token_stack_.top();
    top_cursor_ = std::move(cursor_stack_.top());
    token_stack_.pop();
    cursor_stack_.pop();
  }
}

auto TokenCursor::handle_if() -> void {
  std::vector<Token> tokens;
  while (!top_token_.start_of_line) {
    tokens.push_back(top_token_);
    advance_top_token();
  }
  ExpandCursor cursor(std::move(tokens), context_);
  do {
    auto token = cursor();
    tokens.push_back(token);
  } while (tokens.back().kind != TokenKind::eof);
  int64_t value;
  if (!const_int(tokens, *context_, &value)) {
    context_->fatal(cond_stack_.top(), "");
  }
  if (!value) {
    if_group();
  }
}

auto TokenCursor::handle_ifdef() -> void {
  auto token = top_token_;
  advance_top_token();
  skip_line();
  if (token.kind != TokenKind::identifier) {
    context_->fatal(token.location, "");
  }
  if (!context_->find_macro(token.inner)) {
    if_group();
  }
}

auto TokenCursor::handle_ifndef() -> void {
  auto token = top_token_;
  advance_top_token();
  skip_line();
  if (token.kind != TokenKind::identifier) {
    context_->fatal(token.location, "");
  }
  if (context_->find_macro(token.inner)) {
    if_group();
  }
}

auto TokenCursor::if_group() -> void {
  int depth = 1;
  while (top_token_.kind != TokenKind::eof) {
    if (!top_token_.start_of_line || top_token_.kind != TokenKind::hash) {
      advance_top_token();
      continue;
    }
    advance_top_token();
    if (top_token_.kind != TokenKind::identifier) {
      continue;
    }
    if (top_token_.inner == sb_if || top_token_.inner == sb_ifdef ||
        top_token_.inner == sb_ifndef) {
      ++depth;
      advance_top_token();
      continue;
    }
    if (top_token_.inner == sb_else) {
      advance_top_token();
      if (depth == 1) {
        break;
      }
    } else if (top_token_.inner == sb_elif) {
      advance_top_token();
      std::vector<Token> tokens;
      while (!top_token_.start_of_line) {
        tokens.push_back(top_token_);
        advance_top_token();
      }
      ExpandCursor cursor(std::move(tokens), context_);
      do {
        auto token = cursor();
        tokens.push_back(token);
      } while (tokens.back().kind != TokenKind::eof);
      int64_t value;
      if (!const_int(tokens, *context_, &value)) {
        context_->fatal(cond_stack_.top(), "");
      }
      if (value && depth == 1) {
        break;
      }
    } else if (top_token_.inner == sb_endif) {
      advance_top_token();
      if (--depth == 0) {
        cond_stack_.pop();
        break;
      }
    }
  }
}

static auto join(ParamKind lhs, ParamKind rhs) {
  return static_cast<ParamKind>(static_cast<int>(lhs) | static_cast<int>(rhs));
}

auto TokenCursor::define_macro() -> void {
  if (top_token_.kind != TokenKind::identifier) {
    context_->fatal(top_token_.location, "");
  }
  int name = top_token_.inner;
  advance_top_token();
  if (top_token_.start_of_line || top_token_.leading_space ||
      top_token_.kind != TokenKind::open_paren) {
    std::vector<Token> replace;
    while (!top_token_.start_of_line) {
      replace.push_back(top_token_);
      advance_top_token();
    }
    context_->object_macro(name, std::move(replace));
  } else {
    auto param_map = collect_param();
    std::vector<Token> replace;
    while (!top_token_.start_of_line) {
      if (top_token_.kind == TokenKind::identifier) {
        auto it = param_map.find(top_token_.inner);
        if (it != param_map.end()) {
          top_token_.kind = TokenKind::expand_arg;
          top_token_.inner = it->second;
        }
      }
      replace.push_back(top_token_);
      advance_top_token();
    }
    std::vector<ParamKind> param(param_map.size(), ParamKind::none);
    for (int i = 1; i < replace.size(); i++) {
      using enum TokenKind;
      auto &lhs = replace[i - 1], &rhs = replace[i];
      if (lhs.kind == TokenKind::hash) {
        if (rhs.kind != TokenKind::expand_arg) {
          context_->fatal(rhs.inner, "");
        }
        rhs.kind = TokenKind::raw_arg;
      } else if (lhs.kind == expand_arg && rhs.kind == hash_hash) {
        lhs.kind = TokenKind::raw_arg;
      } else if (lhs.kind == hash_hash && rhs.kind == expand_arg) {
        rhs.kind = TokenKind::raw_arg;
      }
    }
    for (auto &token : replace) {
      if (token.kind == TokenKind::expand_arg) {
        param[token.inner] = join(param[token.inner], ParamKind::expand);
      } else if (token.kind == TokenKind::raw_arg) {
        param[token.inner] = join(param[token.inner], ParamKind::raw);
      }
    }

    context_->function_macro(name, std::move(param), std::move(replace));
  }
}

auto TokenCursor::collect_param() -> std::unordered_map<int, int> {
  int open = top_token_.location;
  advance_top_token();
  if (top_token_.kind == TokenKind::close_paren) {
    advance_top_token();
    return {};
  }
  std::unordered_map<int, int> param_map;
  while (true) {
    if (top_token_.kind == TokenKind::identifier) {
      int index = param_map.size();
      int ident = top_token_.inner;
      if (param_map.contains(ident)) {
        context_->fatal(top_token_.location, "");
      }
      param_map.emplace(ident, index);
      advance_top_token();
    } else {
      assert(false);
    }
    if (top_token_.kind == TokenKind::comma) {
      advance_top_token();
      continue;
    }
    if (top_token_.kind == TokenKind::close_paren) {
      advance_top_token();
      break;
    }
    context_->fatal(top_token_.location, "");
  }
  return param_map;
}

auto TokenCursor::remove_macro() -> void {
  if (top_token_.kind != TokenKind::identifier) {
    context_->fatal(top_token_.location, "");
  }
  int name = top_token_.inner;
  context_->remove_macro(name);
  skip_line();
}

} // namespace lzhcc