#include "lzhcc.h"
#include "lzhcc_lex.h"
#include <cctype>
#include <cassert>

namespace lzhcc {

SourceCursor::SourceCursor(Cursor cursor, Context *context)
    : leading_space_(false), start_of_line_(true), current_(0), location_(0),
      cursor_(std::move(cursor)), context_(context) {
  advance_current();
}

auto SourceCursor::operator()() -> Token {
  while (true) {
    if (std::isspace(current_)) {
      white_space();
    } else {
      break;
    }
  }
  if (current_ == 0) {
    start_of_line_ = true;
    return token(TokenKind::eof, location_);
  }
  if (std::isdigit(current_)) {
    return numeric();
  } else {
    return punctuator();
  }
}

auto SourceCursor::white_space() -> void {
  assert(std::isspace(current_));
  eat_while([](char ch) { return ch != '\n' && std::isspace(ch); });
  leading_space_ = true;
}

static auto is_exp(char e, char s) -> bool {
  bool is_e_or_p = e == 'e' || e == 'E' || e == 'p' || e == 'P';
  bool is_sign = s == '+' || s == '-';
  return is_e_or_p && is_sign;
}

auto SourceCursor::numeric() -> Token {
  std::string text = "";
  int location = location_;
  eat_while([&, last = '\0'](char ch) mutable {
    if (is_exp(last, ch) || ch == '.' || std::isalnum(ch)) {
      text.push_back(last = ch);
      return true;
    } else {
      return false;
    }
  });
  int literal = context_->push_literal(std::move(text));
  return token(TokenKind::numeric, location_, literal);
}

auto SourceCursor::punctuator() -> Token {
  int location = location_;
  switch (current_) {
  case '+':
    advance_current();
    return token(TokenKind::plus, location_);
  case '-':
    advance_current();
    return token(TokenKind::minus, location_);
  }
  context_->fatal(location, "error token");
}

auto SourceCursor::token(TokenKind kind, int location, int inner) -> Token {
  auto token = Token {
    .kind = kind,
    .leading_space = leading_space_,
    .start_of_line = start_of_line_,
    .expand_disable = true,
    .location = location,
    .inner = inner,
  };
  leading_space_ = false;
  start_of_line_ = false;
  return token;
}

auto SourceCursor::advance_current() -> void {
  std::tie(current_, location_) = cursor_();
}

template <class Pred> auto SourceCursor::eat_while(Pred &&pred) -> void {
  while (current_ && pred(current_)) {
    advance_current();
  }
}


} // namespace lzhcc