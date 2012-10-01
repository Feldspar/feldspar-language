{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.Frontend.Tuple where

import QuickAnnotate

import Language.Syntactic
import Language.Syntactic.Frontend.TupleConstrained

import Feldspar.Core.Types
import Feldspar.Core.Constructs.Tuple ()
import Feldspar.Core.Constructs

{-
instance
    ( Syntax a
    , Syntax b
    ) =>
      Syntactic (a,b) FeldDomainAll
  where
    type Internal (a,b) =
        ( Internal a
        , Internal b
        )

    desugar = desugarTup2 typeCtx
    sugar   = sugarTup2 typeCtx

instance
    ( Syntax a
    , Syntax b
    , Syntax c
    ) =>
      Syntactic (a,b,c) FeldDomainAll
  where
    type Internal (a,b,c) =
        ( Internal a
        , Internal b
        , Internal c
        )

    desugar = desugarTup3 typeCtx
    sugar   = sugarTup3 typeCtx

instance
    ( Syntax a
    , Syntax b
    , Syntax c
    , Syntax d
    ) =>
      Syntactic (a,b,c,d) FeldDomainAll
  where
    type Internal (a,b,c,d) =
        ( Internal a
        , Internal b
        , Internal c
        , Internal d
        )

    desugar = desugarTup4 typeCtx
    sugar   = sugarTup4 typeCtx

instance
    ( Syntax a
    , Syntax b
    , Syntax c
    , Syntax d
    , Syntax e
    ) =>
      Syntactic (a,b,c,d,e) FeldDomainAll
  where
    type Internal (a,b,c,d,e) =
        ( Internal a
        , Internal b
        , Internal c
        , Internal d
        , Internal e
        )

    desugar = desugarTup5 typeCtx
    sugar   = sugarTup5 typeCtx

instance
    ( Syntax a
    , Syntax b
    , Syntax c
    , Syntax d
    , Syntax e
    , Syntax f
    ) =>
      Syntactic (a,b,c,d,e,f) FeldDomainAll
  where
    type Internal (a,b,c,d,e,f) =
        ( Internal a
        , Internal b
        , Internal c
        , Internal d
        , Internal e
        , Internal f
        )

    desugar = desugarTup6 typeCtx
    sugar   = sugarTup6 typeCtx

instance
    ( Syntax a
    , Syntax b
    , Syntax c
    , Syntax d
    , Syntax e
    , Syntax f
    , Syntax g
    ) =>
      Syntactic (a,b,c,d,e,f,g) FeldDomainAll
  where
    type Internal (a,b,c,d,e,f,g) =
        ( Internal a
        , Internal b
        , Internal c
        , Internal d
        , Internal e
        , Internal f
        , Internal g
        )

    desugar = desugarTup7 typeCtx
    sugar   = sugarTup7 typeCtx
-}

instance (Syntax a, Syntax b)                                                   => Syntax (a,b)
instance (Syntax a, Syntax b, Syntax c)                                         => Syntax (a,b,c)
instance (Syntax a, Syntax b, Syntax c, Syntax d)                               => Syntax (a,b,c,d)
instance (Syntax a, Syntax b, Syntax c, Syntax d, Syntax e)                     => Syntax (a,b,c,d,e)
instance (Syntax a, Syntax b, Syntax c, Syntax d, Syntax e, Syntax f)           => Syntax (a,b,c,d,e,f)
instance (Syntax a, Syntax b, Syntax c, Syntax d, Syntax e, Syntax f, Syntax g) => Syntax (a,b,c,d,e,f,g)



instance
    ( Annotatable a
    , Annotatable b
    ) =>
      Annotatable (a,b)
  where
    annotate info (a,b) =
        ( annotate (info ++ " (tuple element 1)") a
        , annotate (info ++ " (tuple element 2)") b
        )

instance
    ( Annotatable a
    , Annotatable b
    , Annotatable c
    ) =>
      Annotatable (a,b,c)
  where
    annotate info (a,b,c) =
        ( annotate (info ++ " (tuple element 1)") a
        , annotate (info ++ " (tuple element 2)") b
        , annotate (info ++ " (tuple element 3)") c
        )

instance
    ( Annotatable a
    , Annotatable b
    , Annotatable c
    , Annotatable d
    ) =>
      Annotatable (a,b,c,d)
  where
    annotate info (a,b,c,d) =
        ( annotate (info ++ " (tuple element 1)") a
        , annotate (info ++ " (tuple element 2)") b
        , annotate (info ++ " (tuple element 3)") c
        , annotate (info ++ " (tuple element 4)") d
        )

instance
    ( Annotatable a
    , Annotatable b
    , Annotatable c
    , Annotatable d
    , Annotatable e
    ) =>
      Annotatable (a,b,c,d,e)
  where
    annotate info (a,b,c,d,e) =
        ( annotate (info ++ " (tuple element 1)") a
        , annotate (info ++ " (tuple element 2)") b
        , annotate (info ++ " (tuple element 3)") c
        , annotate (info ++ " (tuple element 4)") d
        , annotate (info ++ " (tuple element 5)") e
        )

instance
    ( Annotatable a
    , Annotatable b
    , Annotatable c
    , Annotatable d
    , Annotatable e
    , Annotatable f
    ) =>
      Annotatable (a,b,c,d,e,f)
  where
    annotate info (a,b,c,d,e,f) =
        ( annotate (info ++ " (tuple element 1)") a
        , annotate (info ++ " (tuple element 2)") b
        , annotate (info ++ " (tuple element 3)") c
        , annotate (info ++ " (tuple element 4)") d
        , annotate (info ++ " (tuple element 5)") e
        , annotate (info ++ " (tuple element 6)") f
        )

instance
    ( Annotatable a
    , Annotatable b
    , Annotatable c
    , Annotatable d
    , Annotatable e
    , Annotatable f
    , Annotatable g
    ) =>
      Annotatable (a,b,c,d,e,f,g)
  where
    annotate info (a,b,c,d,e,f,g) =
        ( annotate (info ++ " (tuple element 1)") a
        , annotate (info ++ " (tuple element 2)") b
        , annotate (info ++ " (tuple element 3)") c
        , annotate (info ++ " (tuple element 4)") d
        , annotate (info ++ " (tuple element 5)") e
        , annotate (info ++ " (tuple element 6)") f
        , annotate (info ++ " (tuple element 7)") g
        )

