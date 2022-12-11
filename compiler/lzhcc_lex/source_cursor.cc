#include "lzhcc.h"
#include "lzhcc_lex.h"
#include <cassert>
#include <cctype>

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
  switch (current_) {
  case '\0':
    start_of_line_ = true;
    return token(TokenKind::eof, location_);
  case '0' ... '9':
    return numeric();
  case '_':
  case 'a' ... 'z':
  case 'A' ... 'Z':
    return identifier();
  default:
    return current_ < 0 ? identifier() : punctuator();
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
  return token(TokenKind::numeric, location, literal);
}

auto SourceCursor::identifier() -> Token {
  std::string text = "";
  int location = location_;
  eat_while([&](char ch) {
    if (ch < 0 || ch == '_' || std::isalnum(ch)) {
      text.push_back(ch);
      return true;
    } else {
      return false;
    }
  });
  int literal = context_->push_identifier(std::move(text));
  return token(TokenKind::identifier, location, literal);
}

auto SourceCursor::punctuator() -> Token {
  int location = location_;
  switch (current_) {
  case '+':
    advance_current();
    return token(TokenKind::plus, location);
  case '-':
    advance_current();
    return token(TokenKind::minus, location);
  case '*':
    advance_current();
    return token(TokenKind::star, location);
  case '/':
    advance_current();
    return token(TokenKind::slash, location);
  case '(':
    advance_current();
    return token(TokenKind::open_paren, location);
  case ')':
    advance_current();
    return token(TokenKind::close_paren, location);
  case ';':
    advance_current();
    return token(TokenKind::semi, location);
  case '=':
    advance_current();
    switch (current_) {
    case '=':
      advance_current();
      return token(TokenKind::equal_equal, location);
    default:
      return token(TokenKind::equal, location);
    }
  case '<':
    advance_current();
    switch (current_) {
    case '=':
      advance_current();
      return token(TokenKind::less_equal, location);
    default:
      return token(TokenKind::less, location);
    }
  case '>':
    advance_current();
    switch (current_) {
    case '=':
      advance_current();
      return token(TokenKind::greater_equal, location);
    default:
      return token(TokenKind::greater, location);
    }
  case '!':
    advance_current();
    switch (current_) {
    case '=':
      advance_current();
      return token(TokenKind::exclaim_equal, location);
    default:
      return token(TokenKind::exclaim, location);
    }
  }
  context_->fatal(location, "error token");
}

auto SourceCursor::token(TokenKind kind, int location, int inner) -> Token {
  auto token = Token{
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