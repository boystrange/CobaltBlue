{-# LANGUAGE ForeignFunctionInterface #-}
{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of Cobalt.                                      ║
│                                                                   ║
│ Cobalt is free software: you can redistribute it and/or modify it ║
│ under the terms of the GNU General Public License as published by ║
│ the Free Software Foundation, either version 3 of the License, or ║
│ (at your option) any later version.                               ║
│                                                                   ║
│ Cobalt is distributed in the hope that it will be useful, but     ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with Cobalt.  If not, see <http://www.gnu.org/licenses/>.   ║
│                                                                   ║
│ Copyright 2016-2017 Luca Padovani                                 ║
╘═══════════════════════════════════════════════════════════════════╝
-}

module Presburger.Lash.Automaton
  (NDD,
   initialize,
   finalize,
   empty,
   universe,
   fromEquation,
   fromInequation,
   dimension,
   isEmpty,
   isEqualTo,
   isDisjointFrom,
   isIncludedIn,
   union,
   Presburger.Lash.Automaton.product,
   intersection,
   difference,
   projection,
   negation,
   expansion,
   sizeOf,
   printAutomaton,
   writeDOT
  )
where

import Data.Int
import Foreign.C
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Control.Monad (unless)

data OpaqueNDD
data NDD = NDD !(ForeignPtr OpaqueNDD)

foreign import ccall "lash_init" c_lash_init :: IO CInt
foreign import ccall "lash_end" c_lash_end :: IO CInt
foreign import ccall "lash_perror" c_lash_perror :: CString -> IO ()
foreign import ccall "my_ndd_dimension" ndd_dimension :: Ptr OpaqueNDD -> IO Int32
foreign import ccall "my_ndd_print_automaton" ndd_print_automaton :: Ptr OpaqueNDD -> IO ()
foreign import ccall "my_ndd_automaton_to_dot" ndd_automaton_to_dot :: Ptr OpaqueNDD -> CString -> IO ()
foreign import ccall "my_ndd_automaton_size_of" ndd_automaton_size_of :: Ptr OpaqueNDD -> IO CInt
foreign import ccall "&ndd_free" ndd_free :: FunPtr (Ptr OpaqueNDD -> IO ())
foreign import ccall "ndd_create_empty" ndd_create_empty :: Int8 -> Int32 -> Int -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_create_zn_msdf" ndd_create_zn_msdf :: Int8 -> Int32 -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_valid_reference" ndd_valid_reference :: Ptr OpaqueNDD -> IO Bool
foreign import ccall "ndd_is_empty" ndd_is_empty :: Ptr OpaqueNDD -> IO Bool
foreign import ccall "ndd_create_equation_msdf" ndd_create_equation_msdf :: Int8 -> Int32 -> Ptr Int32 -> Int32 -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_create_inequation_msdf" ndd_create_inequation_msdf :: Int8 -> Int32 -> Ptr Int32 -> Int32 -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_projection" ndd_projection :: Ptr OpaqueNDD -> Int32 -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_intersection" ndd_intersection :: Ptr OpaqueNDD -> Ptr OpaqueNDD -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_union" ndd_union :: Ptr OpaqueNDD -> Ptr OpaqueNDD -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_product" ndd_product :: Ptr OpaqueNDD -> Ptr OpaqueNDD -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_difference" ndd_difference :: Ptr OpaqueNDD -> Ptr OpaqueNDD -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_interleave_z" ndd_interleave_z :: Ptr OpaqueNDD -> Int32 -> Int32 -> IO (Ptr OpaqueNDD)
foreign import ccall "ndd_inclusion" ndd_inclusion :: Ptr OpaqueNDD -> Ptr OpaqueNDD -> IO Bool
foreign import ccall "ndd_equality" ndd_equality :: Ptr OpaqueNDD -> Ptr OpaqueNDD -> IO Bool
foreign import ccall "ndd_disjoint" ndd_disjoint :: Ptr OpaqueNDD -> Ptr OpaqueNDD -> IO Bool

-- Lash initialization and finalization + utility functions

initialize :: IO ()
initialize = do
  res <- c_lash_init
  unless (res == 0) (withCString "initialize failed" c_lash_perror)

finalize :: IO ()
finalize = do
  res <- c_lash_end
  unless (res == 0) (withCString "finalize failed" c_lash_perror)

-- Utility functions

libraryFail :: String -> IO a
libraryFail msg = do
  withCString "lash" c_lash_perror 
  fail msg

wrap :: String -> Ptr OpaqueNDD -> IO NDD
wrap msg ptr = do
  unless (ptr /= nullPtr) (libraryFail $ msg ++ " returned null pointer")
  fptr <- newForeignPtr ndd_free ptr
  return (NDD fptr)

withForeignPtr2 :: ForeignPtr a -> ForeignPtr b -> (Ptr a -> Ptr b -> IO c) -> IO c
withForeignPtr2 ptr1 ptr2 f = withForeignPtr ptr1 (\ptr1 -> withForeignPtr ptr2 (f ptr1))

-- Construction

empty :: Int -> IO NDD
empty d =
  ndd_create_empty 2 (fromIntegral d) 1
  >>= wrap "empty"

universe :: Int -> IO NDD
universe d =
  ndd_create_zn_msdf 2 (fromIntegral d)
  >>= wrap "universe"

fromEquation :: [Int] -> Int -> IO NDD
fromEquation cs b =
  withArrayLen cs' (\l ptr -> ndd_create_equation_msdf 2 (fromIntegral l) ptr b')
  >>= wrap "equation"
  where
    b' = fromIntegral b
    cs' = map fromIntegral cs

fromInequation :: [Int] -> Int -> IO NDD
fromInequation cs b =
  withArrayLen cs' (\l ptr -> ndd_create_inequation_msdf 2 (fromIntegral l) ptr b')
  >>= wrap "inequation"
  where
    b' = fromIntegral b
    cs' = map fromIntegral cs

-- Automata utility functions

printAutomaton :: NDD -> IO ()
printAutomaton (NDD ptr) = withForeignPtr ptr ndd_print_automaton

writeDOT :: String -> NDD -> IO ()
writeDOT name (NDD ptr) =
  withForeignPtr ptr (\ptr -> withCString name (ndd_automaton_to_dot ptr))

sizeOf :: NDD -> IO Int
sizeOf (NDD ptr) = do
  n <- withForeignPtr ptr ndd_automaton_size_of
  return $ fromIntegral n

-- Queries

dimension :: NDD -> IO Int
dimension (NDD ptr) = do
  dim <- withForeignPtr ptr ndd_dimension
  return $ fromIntegral dim

isEmpty :: NDD -> IO Bool
isEmpty (NDD ptr) = withForeignPtr ptr ndd_is_empty

isIncludedIn :: NDD -> NDD -> IO Bool
isIncludedIn (NDD ptr1) (NDD ptr2) = withForeignPtr2 ptr1 ptr2 ndd_inclusion

isEqualTo :: NDD -> NDD -> IO Bool
isEqualTo (NDD ptr1) (NDD ptr2) = withForeignPtr2 ptr1 ptr2 ndd_equality

isDisjointFrom :: NDD -> NDD -> IO Bool
isDisjointFrom (NDD ptr1) (NDD ptr2) = withForeignPtr2 ptr1 ptr2 ndd_disjoint

-- Transformations

union :: NDD -> NDD -> IO NDD
union (NDD ptr1) (NDD ptr2) =
  withForeignPtr2 ptr1 ptr2 ndd_union
  >>= wrap "union"

product :: NDD -> NDD -> IO NDD
product (NDD ptr1) (NDD ptr2) =
  withForeignPtr2 ptr1 ptr2 ndd_product
  >>= wrap "product"

difference :: NDD -> NDD -> IO NDD
difference (NDD ptr1) (NDD ptr2) =
  withForeignPtr2 ptr1 ptr2 ndd_difference
  >>= wrap "difference"

intersection :: NDD -> NDD -> IO NDD
intersection (NDD ptr1) (NDD ptr2) =
  withForeignPtr2 ptr1 ptr2 ndd_intersection
  >>= wrap "intersection"

projection :: NDD -> Int -> IO NDD
projection (NDD ptr) p =
  withForeignPtr ptr (\ptr -> ndd_projection ptr (fromIntegral p))
  >>= wrap "projection"

negation :: NDD -> IO NDD
negation ndd = do
  dim <- dimension ndd
  u <- universe dim
  difference u ndd

expandRight :: NDD -> IO NDD
expandRight a@(NDD ptr) = do
  dim <- dimension a
  ptr <- withForeignPtr ptr (\ptr -> ndd_interleave_z ptr (fromIntegral dim) 0)
  wrap "expansion" ptr

expansion :: NDD -> Int -> IO NDD
expansion ndd n | n <= 0 = return ndd
                | otherwise = do ndd <- expandRight ndd
                                 expansion ndd (n - 1)
