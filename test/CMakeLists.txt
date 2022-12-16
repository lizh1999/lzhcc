

set(CC riscv64-unknown-linux-gnu-gcc)
set(QEMU qemu-riscv64)
set(TEST_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(LZHCC ${CMAKE_BINARY_DIR}/lzhcc)

macro(create_test name)
  add_custom_command(
    OUTPUT ${name}.s
    COMMAND ${CC} -o- -E -P -C ${name}.c | lzhcc -o ${name}.s -
  )
  add_custom_command(
    OUTPUT ${name}
    COMMAND ${CC} -o ${name} ${name}.s -xc common
  )

  add_test(NAME ${name} COMMAND ${QEMU} ${name})
endmacro()

# create_test(arith)

# add_custom_command(
#   OUTPUT arith.s
#   COMMAND ${CC} -o- -E -P -C arith.c | lzhcc -o arith.s -
# )
# add_custom_command(
#   OUTPUT arith
#   COMMAND ${CC} -o arith arith.s -xc common
# )

message(WARN ${LZHCC})

add_custom_target(arith
  # COMMAND ${CC} -o- -E -P -C ${TEST_DIR}/arith.c | ${LZHCC} -o arith.s -
  # COMMAND ${CC} -o arith.exe arith.s -xc ${TEST_DIR}/common
  # COMMAND ${QEMU} arith.exe
)

add_test(t_arith arith)