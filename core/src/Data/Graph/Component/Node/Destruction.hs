module Data.Graph.Component.Node.Destruction where

import           Prologue hiding (Type)
import qualified Prologue as P

import qualified Data.Graph.Component.Edge.Destruction as Edge
import qualified Data.Graph.Component.Node.Class       as Node
import qualified Data.Graph.Component.Node.Layer       as Node
import qualified Data.Graph.Data.Component.Class       as Component
import qualified Data.Graph.Data.Component.List        as ComponentList
import qualified Data.Graph.Data.Layer.Class           as Layer
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Set                              as Set
import qualified Data.Set.Mutable.Class                as MutableSet

import Data.Graph.Component.Edge.Class (Edge, Edges, Source, Target)
import Data.Graph.Component.Node.Class (Node, Nodes)
import Data.Graph.Component.Node.Layer (Model, Type, Users)
import Data.Graph.Fold.SubComponents   ( SubComponents
                                       , subComponents
                                       , SubComponents1
                                       , subComponents1)


-------------------------
-- === Destruction === --
-------------------------

-- === API === --

type Delete m =
    ( MonadIO m
    , Edge.Delete m
    , Component.Destructor1 m Node
    , SubComponents1 Edges Nodes m
    )

delete :: Delete m => Node layout -> m ()
delete =  \node -> do
    edges <- subComponents1 @Edges node
    ComponentList.mapM_ Edge.delete edges
    Component.destruct1 node
{-# INLINE delete #-}

type DeleteSubtree m =
    ( MonadIO m
    , Delete m
    , Layer.Reader Node Users  m
    , Layer.Reader Node Model  m
    , Layer.Reader Node Type   m
    , Layer.Reader Edge Target m
    , Layer.Reader Edge Source m
    , SubComponents Edges m (Node.Uni ())
    , Layer.IsUnwrapped Node.Uni
    )

deleteSubtreeWithWhitelist :: ∀ layout m. DeleteSubtree m
                           => Set.Set Node.Some -> Node layout -> m ()
deleteSubtreeWithWhitelist whitelist = go . Layout.relayout where
    go :: Node.Some -> m ()
    go root = do
        succs     <- MutableSet.toList =<< Layer.read @Users root
        succEdges <- for succs $ \edge -> (,) <$> Layer.read @Source edge
                                              <*> Layer.read @Target edge
        let allLoops    = all (uncurry (==)) succEdges
            whitelisted = Set.member root whitelist
        when (allLoops && not whitelisted) $ do
            inputEdges <- Node.inputs root
            inputs     <- ComponentList.mapM (Layer.read @Source) inputEdges
            tp         <- Layer.read @Source =<< Layer.read @Type root
            delete root
            traverse_ go (tp : inputs)
{-# INLINE deleteSubtreeWithWhitelist #-}

deleteSubtree :: DeleteSubtree m => Node layout -> m ()
deleteSubtree = deleteSubtreeWithWhitelist mempty
{-# INLINE deleteSubtree #-}
