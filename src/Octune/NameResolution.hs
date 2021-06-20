module Octune.NameResolution where

import           Octune.Types

-- Variables without qualified module names
-- implicitly belong to the module they reside in

resolveModuleVariables :: AST Ann -> AST Ann
resolveModuleVariables (File a moduleName decls) =
    File a moduleName (fmap go decls)
  where
    go :: AST Ann -> AST Ann
    go (Decl ann vName expr)   = Decl ann vName (go expr)
    go (Song ann bpm expr)     = Song ann bpm (go expr)
    go (Var ann qName)         = Var ann (qualifyIfImplicit qName)
    go n@LineNote{}            = n
    go (LineApp ann lFun args) = LineApp ann lFun (fmap go args)
    go b@BeatsAssertion{}      = b
    go _                       = error "Should not be called on Files"

    qualifyIfImplicit :: QualifiedName -> QualifiedName
    qualifyIfImplicit (QualName [] vName) = QualName moduleName vName
    qualifyIfImplicit qName               = qName
resolveModuleVariables _ = error "Should only be called on Files"
