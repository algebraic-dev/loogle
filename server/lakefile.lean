import Lake
open Lake DSL

package server where

lean_lib Server where
  -- add library configuration options here

@[default_target]
lean_exe server where
  root := `Main
