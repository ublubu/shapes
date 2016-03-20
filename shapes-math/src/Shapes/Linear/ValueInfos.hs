{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.ValueInfos where

import GHC.Prim
import GHC.Types (Double(..))

import Shapes.Linear.Template (ValueInfo(..))

doubleInfo :: ValueInfo
doubleInfo = ValueInfo { _valueN = ''Double#
                       , _valueWrap = 'D#
                       , _valueBoxed = ''Double
                       , _valueAdd = '(+##)
                       , _valueSub = '(-##)
                       , _valueMul = '(*##)
                       , _valueDiv = '(/##)
                       , _valueNeg = 'negateDouble#
                       , _valueEq = '(==##)
                       , _valueNeq = '(/=##)
                       , _valueLeq = '(<=##)
                       , _valueGeq = '(>=##)
                       , _valueGt = '(>##)
                       , _valueLt = '(<##)
                       }
