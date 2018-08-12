{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.AWS where

import Prologue

import Data.ByteString  (ByteString)
import Data.Map         (Map)
import Data.Text        (Text)
import Luna.Std.Builder (LTp (..), makeFunctionIO)

import qualified Data.Map                    as Map
import qualified Luna.IR                     as IR
import qualified Luna.Pass.Sourcing.Data.Def as Def
import qualified Luna.Runtime                as Luna
import qualified Luna.Std.Builder            as Builder
import qualified Network.AWS                 as AWS
import qualified Network.AWS.Lambda          as Lambda
import qualified OCI.Data.Name               as Name


type AWSModule = "Std.AWS"

awsModule :: Name.Qualified
awsModule = Name.qualFromSymbol @AWSModule

exports :: forall graph m. Builder.StdBuilder graph m => m (Map IR.Name Def.Def)
exports = do
    let envT        = LCons awsModule "AWSEnv"               []
        invokeRespT = LCons awsModule "LambdaInvokeResponse" []

    let listFunsVal :: AWS.Env -> IO ()
        listFunsVal env = do
          funs <- AWS.runResourceT $ AWS.runAWS env
                                   $ AWS.send
                                   $ Lambda.listFunctions
          print funs
    primListFuns <- makeFunctionIO @graph (flip Luna.toValue listFunsVal)
                                          [envT] Builder.noneLT

    let newEnvVal :: IO AWS.Env
        newEnvVal = AWS.newEnv AWS.Discover
    primAWSNewEnv <- makeFunctionIO @graph (flip Luna.toValue newEnvVal)
                                           [] envT

    let invokeVal :: AWS.Env -> Text -> ByteString -> IO Lambda.InvokeResponse
        invokeVal env fname payload =
            let invocation = Lambda.invoke fname payload
            in AWS.runResourceT . AWS.runAWS env . AWS.send $ invocation
        invokeArgsT = [envT, Builder.textLT, Builder.binaryLT]
    primAWSInvoke <- makeFunctionIO @graph (flip Luna.toValue invokeVal)
                                           invokeArgsT invokeRespT

    pure $ Map.fromList [ ("primAWSListFuns", primListFuns)
                        , ("primAWSNewEnv",   primAWSNewEnv)
                        , ("primAWSInvoke",   primAWSInvoke)
                        ]


type instance Luna.RuntimeRepOf AWS.Env =
    Luna.AsNative ('Luna.ClassRep AWSModule "AWSEnv")
type instance Luna.RuntimeRepOf Lambda.InvokeResponse =
    Luna.AsNative ('Luna.ClassRep AWSModule "LambdaInvokeResponse")
