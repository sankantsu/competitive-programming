add_executable(bayesian main.cc)
target_compile_options(bayesian PUBLIC -fsanitize=address)
