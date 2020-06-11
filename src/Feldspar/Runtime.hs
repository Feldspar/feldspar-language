{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- | A module provinding a linker hook that can be referenced to ensure
-- that the runtime support files are linked.

module Feldspar.Runtime
    ( feldspar_compiler_hook
    )
  where

feldspar_compiler_hook :: Int

#ifdef CABAL_IS_USED

feldspar_compiler_hook = sum [ feldspar_c99_hook
                             , feldspar_ivar_hook
                             , feldspar_taskpool_hook
                             ]

foreign import ccall safe "feldspar_c99_hook"
  feldspar_c99_hook :: Int

foreign import ccall safe "feldspar_ivar_hook"
  feldspar_ivar_hook :: Int

foreign import ccall safe "feldspar_taskpool_hook"
  feldspar_taskpool_hook :: Int

#else

feldspar_compiler_hook = 0

#endif
