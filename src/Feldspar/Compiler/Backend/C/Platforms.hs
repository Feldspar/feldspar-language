--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Feldspar.Compiler.Backend.C.Platforms
    ( availablePlatforms
    , platformFromName
    , c99
    , c99OpenMp
    , c99Wool
    , tic64x
    , extend
    ) where

import Feldspar.Compiler.Imperative.Representation (Type, renderType)
import Feldspar.Compiler.Options

availablePlatforms :: [Platform]
availablePlatforms = [ c99, c99OpenMp, c99Wool, ba, tic64x ]

platformFromName :: String -> Platform
platformFromName str
  = head $ [pf | pf <- availablePlatforms, platformName pf == str]
             ++ error ("platformFromName: No platform named " ++ str)

c99 :: Platform
c99 = Platform {
    platformName = "c99",
    includes =
        [ "feldspar_c99.h"
        , "feldspar_array.h"
        , "feldspar_future.h"
        , "ivar.h"
        , "taskpool.h"
        , "<stdint.h>"
        , "<string.h>"
        , "<math.h>"
        , "<stdbool.h>"
        , "<complex.h>"],
    varFloating = True,
    codeGenerator = "c"
}

c99OpenMp :: Platform
c99OpenMp = c99 { platformName = "c99OpenMp"
                , varFloating = False
                }

c99Wool :: Platform
c99Wool = c99 { platformName = "c99Wool"
              , includes = "wool.h":includes c99
              , varFloating = False
              }

ba :: Platform
ba = c99 { platformName = "ba"
         , codeGenerator = "ba"
         }

tic64x :: Platform
tic64x = Platform {
    platformName = "tic64x",
    includes = [ "feldspar_tic64x.h", "feldspar_array.h", "<c6x.h>", "<string.h>"
               , "<math.h>"],
    varFloating = True,
    codeGenerator = "c"
}

extend :: Platform -> String -> Type -> String
extend Platform{..} s t = s ++ "_fun_" ++ renderType t
