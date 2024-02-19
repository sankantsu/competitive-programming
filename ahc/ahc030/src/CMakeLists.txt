add_executable(main main.cc)
target_compile_options(main PUBLIC -fsanitize=address)
