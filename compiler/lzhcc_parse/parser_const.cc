#include "lzhcc_parse.h"
#include <bit>

namespace lzhcc {

using LabelRef = std::string_view **;

static auto const_addr(Expr *, int64_t *, LabelRef) -> bool;

static auto const_int(Expr *, int64_t *, LabelRef) -> bool;
static auto const_float(Expr *, double *) -> bool;
static auto const_value_float(ValueExpr *, double *) -> bool;
static auto const_value_int(ValueExpr *, int64_t *, LabelRef) -> bool;
static auto const_member_int(MemberExpr *, int64_t *, LabelRef) -> bool;
static auto const_unary_float(UnaryExpr *, double *) -> bool;
static auto const_unary_int(UnaryExpr *, int64_t *, LabelRef) -> bool;
static auto const_binary_float(BinaryExpr *, double *) -> bool;
static auto const_binary_int(BinaryExpr *, int64_t *, LabelRef) -> bool;
static auto const_condition_float(ConditionExpr *, double *) -> bool;
static auto const_condition_int(ConditionExpr *, int64_t *, LabelRef) -> bool;

auto Parser::const_int(int64_t *value) -> bool {
  auto expr = condition();
  return lzhcc::const_int(expr, value, nullptr);
}

auto Parser::const_int(Expr *expr, int64_t *value, LabelRef label) -> bool {
  return lzhcc::const_int(expr, value, label);
}

auto Parser::const_float(Expr *expr, double *value) -> bool {
  return lzhcc::const_float(expr, value);
}

auto const_int(Expr *expr, int64_t *value, LabelRef label) -> bool {
  if (expr->type->kind == TypeKind::floating) {
    double floating;
    if (!const_float(expr, &floating)) {
      return false;
    }
    *value = floating;
    return true;
  }
  switch (expr->kind) {
  case ExperKind::integer:
    *value = cast<IntegerExpr>(expr)->value;
    return true;
  case ExperKind::unary:
    return const_unary_int(cast<UnaryExpr>(expr), value, label);
  case ExperKind::binary:
    return const_binary_int(cast<BinaryExpr>(expr), value, label);
  case ExperKind::condition:
    return const_condition_int(cast<ConditionExpr>(expr), value, label);
  case ExperKind::value:
    return const_value_int(cast<ValueExpr>(expr), value, label);
  case ExperKind::member:
    return const_member_int(cast<MemberExpr>(expr), value, label);
  case ExperKind::zero:
  case ExperKind::call:
  case ExperKind::stmt:
  case ExperKind::floating:
    return false;
  }
}

auto const_float(Expr *expr, double *value) -> bool {
  if (expr->type->kind == TypeKind::integer) {
    int64_t integer;
    if (!const_int(expr, &integer, nullptr)) {
      return false;
    }
    if (cast<IntegerType>(expr->type)->sign == Sign::unsign) {
      *value = std::bit_cast<uint64_t>(integer);
    } else {
      *value = integer;
    }
    return true;
  }
  switch (expr->kind) {
  case ExperKind::floating:
    *value = cast<FloatingExpr>(expr)->value;
    return true;
  case ExperKind::unary:
    return const_unary_float(cast<UnaryExpr>(expr), value);
  case ExperKind::binary:
    return const_binary_float(cast<BinaryExpr>(expr), value);
  case ExperKind::condition:
    return const_condition_float(cast<ConditionExpr>(expr), value);
  case ExperKind::zero:
  case ExperKind::value:
  case ExperKind::integer:
  case ExperKind::call:
  case ExperKind::stmt:
  case ExperKind::member:
    return false;
  }
}

auto const_value_int(ValueExpr *expr, int64_t *offset, LabelRef label) -> bool {
  auto value = expr->value;
  if (value->kind != ValueKind::global) {
    return false;
  }
  auto type_kind = value->type->kind;
  if (!label ||
      type_kind != TypeKind::array && type_kind != TypeKind::function) {
    return false;
  }
  *offset = 0;
  *label = &cast<GValue>(value)->name;
  return true;
}

auto const_member_int(MemberExpr *expr, int64_t *offset, LabelRef label)
    -> bool {
  if (!label || expr->type->kind != TypeKind::array) {
    return false;
  } else if (const_addr(expr->record, offset, label)) {
    offset += expr->offset;
    return true;
  } else {
    return false;
  }
}

static auto const_cast_int(int64_t *value, Type *type) -> bool {
  if (type->kind == TypeKind::pointer) {
    return true;
  } else if (type->kind != TypeKind::integer) {
    return false;
  }
  auto integer = cast<IntegerType>(type);
  switch (pattern(integer->kind, integer->sign)) {
  case Scalar::int8:
    *value = static_cast<int8_t>(*value);
    return true;
  case Scalar::int16:
    *value = static_cast<int16_t>(*value);
    return true;
  case Scalar::int32:
    *value = static_cast<int32_t>(*value);
    return true;
  case Scalar::int64:
    *value = static_cast<int64_t>(*value);
    return true;
  case Scalar::uint8:
    *value = static_cast<uint8_t>(*value);
    return true;
  case Scalar::uint16:
    *value = static_cast<uint16_t>(*value);
    return true;
  case Scalar::uint32:
    *value = static_cast<uint32_t>(*value);
    return true;
  case Scalar::uint64:
    *value = static_cast<uint64_t>(*value);
    return true;
  }
}

auto const_unary_float(UnaryExpr *expr, double *value) -> bool {
  switch (expr->kind) {
  case UnaryKind::negative:
    if (const_float(expr->operand, value)) {
      *value = -*value;
      return true;
    } else {
      return false;
    }
  case UnaryKind::cast:
    return const_float(expr->operand, value);
  case UnaryKind::refrence:
  case UnaryKind::deref:
  case UnaryKind::logical_not:
  case UnaryKind::bitwise_not:
    return false;
  }
}

auto const_unary_int(UnaryExpr *expr, int64_t *value, LabelRef label) -> bool {
  switch (expr->kind) {
  case UnaryKind::negative:
    if (const_int(expr->operand, value, nullptr)) {
      *value = -*value;
      return true;
    } else {
      return false;
    }
  case UnaryKind::cast:
    if (const_int(expr->operand, value, label)) {
      return const_cast_int(value, expr->type);
    } else {
      return false;
    }
  case UnaryKind::logical_not:
    if (const_int(expr->operand, value, nullptr)) {
      *value = !*value;
      return true;
    } else {
      return false;
    }
  case UnaryKind::bitwise_not:
    if (const_int(expr->operand, value, nullptr)) {
      *value = ~*value;
      return true;
    } else {
      return false;
    }
  case UnaryKind::refrence:
    return label ? const_addr(expr->operand, value, label) : false;
  case UnaryKind::deref:
    return false;
  }
}

auto const_binary_int(BinaryExpr *expr, int64_t *value, LabelRef label)
    -> bool {
#define eval(op, x)                                                            \
  ({                                                                           \
    bool success = false;                                                      \
    int64_t lhs, rhs;                                                          \
    if (const_int(expr->lhs, &lhs, x) && const_int(expr->rhs, &rhs, 0)) {      \
      if (cast<IntegerType>(expr->lhs->type)->sign == Sign::unsign) {          \
        *value = (uint64_t)lhs op rhs;                                         \
      } else {                                                                 \
        *value = lhs op rhs;                                                   \
      }                                                                        \
      success = true;                                                          \
    }                                                                          \
    success;                                                                   \
  })
  switch (expr->kind) {
  case BinaryKind::add:
    return eval(+, label);
  case BinaryKind::subtract:
    return eval(-, label);
  case BinaryKind::multiply:
    return eval(*, nullptr);
  case BinaryKind::divide:
    return eval(/, nullptr);
  case BinaryKind::modulo:
    return eval(%, nullptr);
  case BinaryKind::less_than:
    return eval(<, nullptr);
  case BinaryKind::less_equal:
    return eval(<=, nullptr);
  case BinaryKind::equal:
    return eval(==, nullptr);
  case BinaryKind::not_equal:
    return eval(!=, nullptr);
  case BinaryKind::bitwise_or:
    return eval(|, nullptr);
  case BinaryKind::bitwise_xor:
    return eval(^, nullptr);
  case BinaryKind::bitwise_and:
    return eval(&, nullptr);
  case BinaryKind::logical_and:
    return eval(&&, nullptr);
  case BinaryKind::logical_or:
    return eval(||, nullptr);
  case BinaryKind::shift_left:
    return eval(<<, nullptr);
  case BinaryKind::shift_right:
    return eval(>>, nullptr);
  case BinaryKind::comma:
    return const_int(expr->rhs, value, label);
  case BinaryKind::assign:
    return false;
  }
#undef eval
}

auto const_binary_float(BinaryExpr *expr, double *value) -> bool {
#define eval(op)                                                               \
  ({                                                                           \
    bool success = false;                                                      \
    double lhs, rhs;                                                           \
    if (const_float(expr->lhs, &lhs) && const_float(expr->rhs, &rhs)) {        \
      *value = lhs op rhs;                                                     \
      success = true;                                                          \
    }                                                                          \
    success;                                                                   \
  })
  switch (expr->kind) {
  case BinaryKind::add:
    return eval(+);
  case BinaryKind::subtract:
    return eval(-);
  case BinaryKind::multiply:
    return eval(*);
  case BinaryKind::divide:
    return eval(/);
  case BinaryKind::comma:
    return const_float(expr->rhs, value);
  case BinaryKind::modulo:
  case BinaryKind::less_than:
  case BinaryKind::less_equal:
  case BinaryKind::equal:
  case BinaryKind::not_equal:
  case BinaryKind::assign:
  case BinaryKind::bitwise_or:
  case BinaryKind::bitwise_xor:
  case BinaryKind::bitwise_and:
  case BinaryKind::logical_and:
  case BinaryKind::logical_or:
  case BinaryKind::shift_left:
  case BinaryKind::shift_right:
    return false;
  }
#undef eval
}

auto const_condition_int(ConditionExpr *expr, int64_t *value, LabelRef label)
    -> bool {
  if (!const_int(expr->cond, value, nullptr)) {
    return false;
  } else {
    return *value ? const_int(expr->then, value, label)
                  : const_int(expr->else_, value, label);
  }
}

auto const_condition_float(ConditionExpr *expr, double *value) -> bool {
  if (int64_t tmp; !const_int(expr->cond, &tmp, nullptr)) {
    return false;
  } else {
    return tmp ? const_float(expr->then, value)
               : const_float(expr->else_, value);
  }
}

auto const_addr(Expr *expr, int64_t *offset, LabelRef label) -> bool {
  if (expr->kind == ExperKind::value) {
    auto value = cast<ValueExpr>(expr)->value;
    if (value->kind != ValueKind::global) {
      return false;
    }
    *label = &cast<GValue>(value)->name;
    *offset = 0;
    return true;
  }
  if (expr->kind == ExperKind::unary) {
    auto unary = cast<UnaryExpr>(expr);
    if (unary->kind == UnaryKind::deref) {
      return const_int(unary->operand, offset, label);
    } else {
      return false;
    }
  }

  if (expr->kind == ExperKind::member) {
    auto member = cast<MemberExpr>(expr);
    if (const_addr(member->record, offset, label)) {
      *offset += member->offset;
      return true;
    } else {
      return false;
    }
  }

  return false;
}

} // namespace lzhcc