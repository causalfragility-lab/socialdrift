# Suppress R CMD check NOTE: "no visible binding for global variable '.data'"
# The .data pronoun is provided by rlang/dplyr and is legitimate in tidy eval.
utils::globalVariables(".data")
