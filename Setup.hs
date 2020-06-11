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

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.GHC
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Types
import Distribution.Verbosity (verbose)
import Distribution.PackageDescription

import System.Process ( readProcessWithExitCode )
import System.FilePath ( replaceExtension )
import Control.Monad ( unless )

main = defaultMainWithHooks simpleUserHooks{ buildHook = buildH }

-- | Custom build hook that builds C-sources for benchmarks with x-cc-name set.
buildH :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildH pd lbi user_hooks flags = do
    benchmarks' <- mapM (checkIfAndCompile lbi) $ benchmarks pd
    -- Build the remaining things the regular way.
    buildHook simpleUserHooks pd{ benchmarks = benchmarks' } lbi user_hooks flags
    return ()

-- | Checks if x-cc-name is set, and compiles c-sources with that compiler name.
checkIfAndCompile :: LocalBuildInfo -> Benchmark -> IO Benchmark
checkIfAndCompile lbi bench = do
    let bench_bi  = benchmarkBuildInfo bench
    case lookup "x-cc-name" $ customFieldsBI bench_bi of
        Nothing -> return bench
        Just cc_name  -> do
            let c_srcs    = cSources bench_bi
                cc_opts   = ccOptions bench_bi
                inc_dirs  = includeDirs bench_bi
            -- Compile C/C++ sources
            putStrLn "Invoking icc compiler"
            mapM_ (compile lbi bench cc_name cc_opts inc_dirs) c_srcs
            -- Remove C source code from the hooked build (don't change libs)
            return $ bench{ benchmarkBuildInfo = bench_bi{ cSources = [] } }

-- | Compiles a C file with the given options.
compile :: LocalBuildInfo -> Benchmark -> String -> [String] -> [String] -> FilePath -> IO ()
compile lbi bench cc_name opts inc_dirs srcfile = do
    let args = [ "-optc -std=c99"
               , "-optc -Wall"
               , "-w"
               , "-c"
               , "-pgmc " ++ cc_name
               ] ++ map ("-optc " ++) opts
        objfile = replaceExtension srcfile "o"
        fullargs = args ++ ["-o", objfile, srcfile]
    (ghcProg,_) <- requireProgram verbose ghcProgram (withPrograms lbi)
    let ghc = programPath ghcProg
    print $ unwords $ ["Calling:",ghc] ++ fullargs
    (_, stdout, stderr) <- readProcessWithExitCode ghc fullargs ""
    let output = stdout ++ stderr
    unless (null output) $ putStrLn output
