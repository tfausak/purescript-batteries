-- | 🔋 A PureScript prelude with more features.
-- |
-- | Unfortunately the generated documentation does not include anything
-- | useful. To learn more about Batteries, please see [the README][].
-- |
-- | [the readme]: https://github.com/tfausak/purescript-batteries#readme
module Batteries
  ( module Batteries
  , module Control.Alt
  , module Control.Alternative
  , module Control.Apply
  , module Control.Arrow
  , module Control.Arrow.Cokleisli
  , module Control.Arrow.Kleisli
  , module Control.Arrow.Static
  , module Control.Biapplicative
  , module Control.Biapply
  , module Control.Bind
  , module Control.Comonad
  , module Control.Comonad.Cofree
  , module Control.Comonad.Env
  , module Control.Comonad.Env.Class
  , module Control.Comonad.Env.Trans
  , module Control.Comonad.Store
  , module Control.Comonad.Store.Class
  , module Control.Comonad.Store.Trans
  , module Control.Comonad.Traced
  , module Control.Comonad.Traced.Class
  , module Control.Comonad.Traced.Trans
  , module Control.Comonad.Trans
  , module Control.Extend
  , module Control.Lazy
  , module Control.Monad
  , module Control.Monad.Aff
  , module Control.Monad.Aff.AVar
  , module Control.Monad.Aff.Class
  , module Control.Monad.Aff.Console
  , module Control.Monad.Aff.Par
  , module Control.Monad.Aff.Unsafe
  , module Control.Monad.Cont.Class
  , module Control.Monad.Cont.Trans
  , module Control.Monad.Eff
  , module Control.Monad.Eff.Class
  , module Control.Monad.Eff.Console
  , module Control.Monad.Eff.Console.Unsafe
  , module Control.Monad.Eff.Exception
  , module Control.Monad.Eff.Random
  , module Control.Monad.Eff.Ref
  , module Control.Monad.Eff.Ref.Unsafe
  , module Control.Monad.Eff.Unsafe
  , module Control.Monad.Error.Class
  , module Control.Monad.Except
  , module Control.Monad.Except.Trans
  , module Control.Monad.Free
  , module Control.Monad.List.Trans
  , module Control.Monad.Maybe.Trans
  , module Control.Monad.RWS
  , module Control.Monad.RWS.Class
  , module Control.Monad.RWS.Trans
  , module Control.Monad.Reader
  , module Control.Monad.Reader.Class
  , module Control.Monad.Reader.Trans
  , module Control.Monad.Rec.Class
  , module Control.Monad.ST
  , module Control.Monad.State
  , module Control.Monad.State.Class
  , module Control.Monad.State.Trans
  , module Control.Monad.Trampoline
  , module Control.Monad.Trans
  , module Control.Monad.Writer
  , module Control.Monad.Writer.Class
  , module Control.Monad.Writer.Trans
  , module Control.MonadPlus
  , module Control.Parallel
  , module Control.Plus
  , module Data.Array
  , module Data.Array.ST
  , module Data.Array.Unsafe
  , module Data.Bifoldable
  , module Data.Bifunctor
  , module Data.Bifunctor.Clown
  , module Data.Bifunctor.Flip
  , module Data.Bifunctor.Join
  , module Data.Bifunctor.Joker
  , module Data.Bifunctor.Product
  , module Data.Bifunctor.Wrap
  , module Data.Bitraversable
  , module Data.CatList
  , module Data.CatQueue
  , module Data.Char
  , module Data.Comparison
  , module Data.Const
  , module Data.Coyoneda
  , module Data.Date
  , module Data.Date.Locale
  , module Data.Date.UTC
  , module Data.Distributive
  , module Data.Either
  , module Data.Either.Nested
  , module Data.Either.Unsafe
  , module Data.Enum
  , module Data.Equivalence
  , module Data.Exists
  , module Data.FingerTree
  , module Data.Foldable
  , module Data.Foreign
  , module Data.Foreign.Class
  , module Data.Foreign.Index
  , module Data.Foreign.Keys
  , module Data.Foreign.Null
  , module Data.Foreign.NullOrUndefined
  , module Data.Foreign.Undefined
  , module Data.Function
  , module Data.Functor
  , module Data.Functor.Contravariant
  , module Data.Functor.Contravariant.Divisible
  , module Data.Functor.Coproduct
  , module Data.Functor.Invariant
  , module Data.Generic
  , module Data.Identity
  , module Data.Inject
  , module Data.Int
  , module Data.Int.Bits
  , module Data.Lazy
  , module Data.List
  , module Data.List.Lazy
  , module Data.List.Unsafe
  , module Data.List.ZipList
  , module Data.Map
  , module Data.Maybe
  , module Data.Maybe.First
  , module Data.Maybe.Last
  , module Data.Maybe.Unsafe
  , module Data.Monoid
  , module Data.Monoid.Additive
  , module Data.Monoid.Conj
  , module Data.Monoid.Disj
  , module Data.Monoid.Dual
  , module Data.Monoid.Endo
  , module Data.Monoid.Multiplicative
  , module Data.NaturalTransformation
  , module Data.Nullable
  , module Data.Op
  , module Data.Predicate
  , module Data.Profunctor
  , module Data.Profunctor.Choice
  , module Data.Profunctor.Star
  , module Data.Profunctor.Strong
  , module Data.Semiring.Free
  , module Data.Sequence
  , module Data.Sequence.NonEmpty
  , module Data.Sequence.Ordered
  , module Data.Set
  , module Data.StrMap
  , module Data.StrMap.ST
  , module Data.StrMap.ST.Unsafe
  , module Data.StrMap.Unsafe
  , module Data.String
  , module Data.String.Regex
  , module Data.String.Unsafe
  , module Data.These
  , module Data.Time
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Data.Unfoldable
  , module Data.Validation
  , module Data.Validation.Semiring
  , module Data.Yoneda
  , module Global
  , module Global.Unsafe
  , module Math
  , module Optic.Core
  , module Optic.Getter
  , module Optic.Laws.Lens
  , module Optic.Lens
  , module Optic.Prism
  , module Optic.Setter
  , module Optic.Types
  , module Prelude
  , module Test.Assert
  , module Type.Proxy
  , module Unsafe.Coerce
  ) where

import Control.Alt
  ( Alt
  , alt
  , (<|>)
  )
import Control.Alternative
  ( Alternative
  )
import Control.Apply
  ( lift2
  , lift3
  , lift4
  , lift5
  , (*>)
  , (<*)
  )
import Control.Arrow
  ( Arrow
  , ArrowPlus
  , ArrowZero
  , aplus
  , azero
  , (<+>)
  )
import Control.Arrow.Cokleisli
  ( Cokleisli(Cokleisli)
  , runCokleisli
  )
import Control.Arrow.Kleisli
  ( Kleisli(Kleisli)
  , runKleisli
  )
import Control.Arrow.Static
  ( Static(Static)
  , runStatic
  )
import Control.Biapplicative
  ( Biapplicative
  )
import Control.Biapply
  ( Biapply
  , bilift2
  , bilift3
  , (*>>)
  , (<<*)
  , (<<*>>)
  , (<<$>>)
  )
import Control.Bind
  ( ifM
  , join
  , (<=<)
  , (=<<)
  , (>=>)
  )
import Control.Comonad
  ( Comonad
  , extract
  )
import Control.Comonad.Cofree
  ( Cofree()
  -- , head -- TODO
  , mkCofree
  -- , tail -- TODO
  )
import Control.Comonad.Env
  ( Env()
  , env
  , mapEnv
  , runEnv
  , withEnv
  )
import Control.Comonad.Env.Class
  ( ComonadEnv
  -- , ask -- TODO
  , asks
  -- , local -- TODO
  )
import Control.Comonad.Env.Trans
  ( EnvT(EnvT)
  , mapEnvT
  , runEnvT
  , withEnvT
  )
import Control.Comonad.Store
  ( Store()
  , runStore
  , store
  )
import Control.Comonad.Store.Class
  ( ComonadStore
  , experiment
  -- , peek -- TODO
  , peeks
  , pos
  , seek
  , seeks
  )
import Control.Comonad.Store.Trans
  ( StoreT(StoreT)
  , runStoreT
  )
import Control.Comonad.Traced
  ( Traced()
  , runTraced
  , traced
  )
import Control.Comonad.Traced.Class
  ( ComonadTraced
  -- , censor -- TODO
  -- , listen -- TODO
  -- , listens -- TODO
  , track
  , tracks
  )
import Control.Comonad.Traced.Trans
  ( TracedT(TracedT)
  , runTracedT
  )
import Control.Comonad.Trans
  ( ComonadTrans
  , lower
  )
import Control.Extend
  ( Extend
  , duplicate
  , extend
  , (<<=)
  , (=<=)
  , (=>=)
  , (=>>)
  )
import Control.Lazy
  ( Lazy
  , defer
  , fix
  )
import Control.Monad
  ( unless
  , when
  )
import Control.Monad.Aff
  ( Aff()
  , Canceler(Canceler)
  , PureAff()
  , apathize
  , attempt
  , cancel
  , cancelWith
  , finally
  , forkAff
  , later
  , later'
  , launchAff
  , liftEff'
  , makeAff
  , makeAff'
  , nonCanceler
  , runAff
  )
import Control.Monad.Aff.AVar
  ( AffAVar()
  , AVAR()
  , AVar()
  , killVar
  , makeVar
  , makeVar'
  , modifyVar
  , putVar
  , takeVar
  )
import Control.Monad.Aff.Class
  ( MonadAff
  , liftAff
  )
import Control.Monad.Aff.Console
  ( -- log -- NOTE: Math
  -- , print -- NOTE: Control.Monad.Eff.Console
  )
import Control.Monad.Aff.Par
  ( Par(Par)
  , runPar
  )
import Control.Monad.Aff.Unsafe
  ( unsafeInterleaveAff
  , unsafeTrace
  )
import Control.Monad.Cont.Class
  ( MonadCont
  , callCC
  )
import Control.Monad.Cont.Trans
  ( ContT(ContT)
  , mapContT
  , runContT
  , withContT
  )
import Control.Monad.Eff
  ( Eff()
  , Pure()
  , forE
  , foreachE
  , runPure
  , untilE
  , whileE
  )
import Control.Monad.Eff.Class
  ( MonadEff
  , liftEff
  )
import Control.Monad.Eff.Console
  ( CONSOLE()
  -- , error -- NOTE: Control.Monad.Eff.Exception
  -- , log -- NOTE: Math
  , print
  )
import Control.Monad.Eff.Console.Unsafe
  ( errorAny
  , logAny
  )
import Control.Monad.Eff.Exception
  ( EXCEPTION()
  , Error()
  , catchException
  , error
  , message
  , throwException
  )
import Control.Monad.Eff.Random
  ( RANDOM()
  , random
  , randomBool
  , randomInt
  , randomRange
  )
import Control.Monad.Eff.Ref
  ( REF()
  , Ref()
  , modifyRef
  , modifyRef'
  , newRef
  , readRef
  , writeRef
  )
import Control.Monad.Eff.Ref.Unsafe
  ( unsafeRunRef
  )
import Control.Monad.Eff.Unsafe
  ( unsafeInterleaveEff
  )
import Control.Monad.Error.Class
  ( MonadError
  , catchError
  , catchJust
  , throwError
  )
import Control.Monad.Except
  ( Except()
  , except
  , mapExcept
  , runExcept
  , withExcept
  )
import Control.Monad.Except.Trans
  ( ExceptT(ExceptT)
  , mapExceptT
  , runExceptT
  , withExceptT
  )
import Control.Monad.Free
  ( Free()
  , foldFree
  , injF
  , liftF
  , liftFI
  , mapF
  , runFree
  , runFreeM
  , suspendF
  )
import Control.Monad.List.Trans
  ( ListT()
  -- , catMaybes -- TODO
  -- , cons -- TODO
  -- , drop -- TODO
  -- , dropWhile -- TODO
  -- , filter -- TODO
  -- , foldl -- NOTE: Data.Foldable
  -- , foldl' -- NOTE: Data.Foldable
  , fromEffect
  -- , head -- TODO
  -- , iterate -- TODO
  -- , mapMaybe -- TODO
  -- , nil -- TODO
  , prepend
  , prepend'
  -- , repeat -- TODO
  -- , scanl -- NOTE: Data.Traversable
  -- , singleton -- NOTE: Data.Unfoldable
  -- , tail -- TODO
  -- , take -- TODO
  -- , takeWhile -- TODO
  -- , uncons -- TODO
  , unfold
  , wrapEffect
  , wrapLazy
  -- , zipWith -- TODO
  , zipWith'
  )
import Control.Monad.Maybe.Trans
  ( MaybeT(MaybeT)
  , mapMaybeT
  , runMaybeT
  )
import Control.Monad.RWS
  ( RWS()
  , evalRWS
  , execRWS
  , mapRWS
  , runRWS
  , rws
  , withRWS
  )
import Control.Monad.RWS.Class
  ( MonadRWS
  )
import Control.Monad.RWS.Trans
  ( RWSResult(RWSResult)
  , RWST(RWST)
  , evalRWST
  , execRWST
  , mapRWST
  , runRWST
  , withRWST
  )
import Control.Monad.Reader
  ( Reader()
  , mapReader
  , runReader
  , withReader
  )
import Control.Monad.Reader.Class
  ( MonadReader
  -- , ask -- TODO
  -- , local -- TODO
  , reader
  )
import Control.Monad.Reader.Trans
  ( ReaderT(ReaderT)
  , mapReaderT
  , runReaderT
  , withReaderT
  )
import Control.Monad.Rec.Class
  ( MonadRec
  , forever
  , tailRec
  , tailRecM
  , tailRecM2
  , tailRecM3
  )
import Control.Monad.ST
  ( ST()
  , STRef()
  , modifySTRef
  , newSTRef
  , pureST
  , readSTRef
  , runST
  , writeSTRef
  )
import Control.Monad.State
  ( State()
  , evalState
  , execState
  , mapState
  , runState
  , withState
  )
import Control.Monad.State.Class
  ( MonadState
  , get
  , gets
  , modify
  , put
  , state
  )
import Control.Monad.State.Trans
  ( StateT(StateT)
  , evalStateT
  , execStateT
  , mapStateT
  , runStateT
  , withStateT
  )
import Control.Monad.Trampoline
  ( Trampoline()
  , delay
  , delay'
  , done
  , runTrampoline
  , suspend
  )
import Control.Monad.Trans
  ( MonadTrans
  , lift
  )
import Control.Monad.Writer
  ( Writer()
  , execWriter
  , mapWriter
  , runWriter
  )
import Control.Monad.Writer.Class
  ( MonadWriter
  -- , censor -- TODO
  -- , listen -- TODO
  -- , listens -- TODO
  , pass
  , tell
  , writer
  )
import Control.Monad.Writer.Trans
  ( WriterT(WriterT)
  , execWriterT
  , mapWriterT
  , runWriterT
  )
import Control.MonadPlus
  ( MonadPlus
  , guard
  )
import Control.Parallel
  ( Parallel()
  , inParallel
  , par
  , race
  , runParallel
  , runParallelWith
  , withCallback
  )
import Control.Plus
  ( Plus
  , empty
  )
import Data.Array
  ( -- alterAt -- TODO
  -- , catMaybes -- TODO
  -- , concat -- TODO
  -- , concatMap -- TODO
  -- , cons -- TODO
  -- , delete -- TODO
  -- , deleteAt -- TODO
  -- , deleteBy -- TODO
  -- , drop -- TODO
  -- , dropWhile -- TODO
  -- , elemIndex -- TODO
  -- , elemLastIndex -- TODO
  -- , filter -- TODO
  -- , filterM -- TODO
  -- , findIndex -- TODO
  -- , findLastIndex -- TODO
  -- , foldM -- TODO
  -- , group -- TODO
  -- , group' -- TODO
  -- , groupBy -- TODO
  -- , head -- TODO
  -- , index -- TODO
  -- , init -- TODO
  -- , insert -- TODO
  -- , insertAt -- TODO
  -- , insertBy -- TODO
  -- , intersect -- TODO
  -- , intersectBy -- TODO
  -- , last -- TODO
  -- , length -- TODO
  -- , many -- TODO
  -- , mapMaybe -- TODO
  -- , modifyAt -- TODO
  -- , nub -- TODO
  -- , nubBy -- TODO
  -- , null -- TODO
  -- , range -- TODO
  -- , replicate -- NOTE: Data.Unfoldable
  -- , replicateM -- TODO
  -- , reverse -- TODO
  -- , singleton -- NOTE: Data.Unfoldable
  -- , slice -- TODO
  -- , snoc -- TODO
  -- , some -- TODO
  -- , sort -- TODO
  -- , sortBy -- TODO
  -- , span -- TODO
  -- , tail -- TODO
  -- , take -- TODO
  -- , takeWhile -- TODO
  -- , uncons -- TODO
  -- , union -- TODO
  -- , unionBy -- TODO
  -- , unzip -- TODO
  -- , updateAt -- TODO
  -- , zip -- TODO
  -- , zipWith -- TODO
  -- , zipWithA -- TODO
  -- , (:) -- TODO
  -- , (!!) -- TODO
  -- , (..) -- TODO
  -- , (\\) -- TODO
  )
import Data.Array.ST
  ( Assoc()
  , STArray()
  , emptySTArray
  , freeze
  , peekSTArray
  , pokeSTArray
  , pushAllSTArray
  , pushSTArray
  , runSTArray
  , spliceSTArray
  , thaw
  , toAssocArray
  )
import Data.Array.Unsafe
  ( -- unsafeIndex -- TODO
  -- , head -- TODO
  -- , init -- TODO
  -- , last -- TODO
  -- , tail -- TODO
  )
import Data.Bifoldable
  ( Bifoldable
  , biall
  , biany
  , bifold
  , bifoldl
  , bifoldMap
  , bifoldr
  , bifor_
  , bisequence_
  , bitraverse_
  )
import Data.Bifunctor
  ( Bifunctor
  , lmap
  , rmap
  )
import Data.Bifunctor.Clown
  ( Clown(Clown)
  , runClown
  )
import Data.Bifunctor.Flip
  ( Flip(Flip)
  , runFlip
  )
import Data.Bifunctor.Join
  ( Join(Join)
  , runJoin
  )
import Data.Bifunctor.Joker
  ( Joker(Joker)
  , runJoker
  )
import Data.Bifunctor.Product
  ( Product(Pair)
  )
import Data.Bifunctor.Wrap
  ( Wrap(Wrap)
  , unwrap
  )
import Data.Bitraversable
  ( Bitraversable
  , bifor
  , bisequence
  , bitraverse
  )
import Data.CatList
  ( CatList(CatCons, CatNil)
  -- , append -- NOTE: Prelude
  -- , cons -- TODO
  -- , empty -- NOTE: Control.Plus
  -- , null -- TODO
  -- , snoc -- TODO
  -- , uncons -- TODO
  )
import Data.CatQueue
  ( CatQueue(CatQueue)
  -- , empty -- NOTE: Control.Plus
  -- , null -- TODO
  -- , snoc -- TODO
  -- , uncons -- TODO
  )
import Data.Char
  ( fromCharCode
  , toCharCode
  -- , toLower -- NOTE: Data.String
  , toString
  -- , toUpper -- NOTE: Data.String
  )
import Data.Comparison
  ( Comparison(Comparison)
  , defaultComparison
  , runComparison
  )
import Data.Const
  ( Const(Const)
  , getConst
  )
import Data.Coyoneda
  ( Coyoneda(Coyoneda)
  , CoyonedaF(CoyonedaF)
  , coyoneda
  , liftCoyoneda
  , liftCoyonedaT
  , liftCoyonedaTF
  , lowerCoyoneda
  )
import Data.Date
  ( Date()
  , DayOfMonth(DayOfMonth)
  , DayOfWeek(Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday)
  , JSDate()
  , LocaleOffset(LocaleOffset)
  , Month(April, August, December, February, January, July, June, March, May, November, October, September)
  , Now()
  , Year(Year)
  , fromEpochMilliseconds
  , fromJSDate
  -- , fromString -- TODO
  , fromStringStrict
  , now
  , nowEpochMilliseconds
  , timezoneOffset
  , toEpochMilliseconds
  , toJSDate
  )
import Data.Date.Locale
  ( Locale()
  -- , date -- NOTE: Data.Date.UTC
  -- , dateTime -- NOTE: Data.Date.UTC
  -- , dayOfMonth -- NOTE: Data.Date.UTC
  -- , dayOfWeek -- NOTE: Data.Date.UTC
  -- , hourOfDay -- NOTE: Data.Date.UTC
  -- , millisecondOfSecond -- NOTE: Data.Date.UTC
  -- , minuteOfHour -- NOTE: Data.Date.UTC
  -- , month -- NOTE: Data.Date.UTC
  -- , secondOfMinute -- NOTE: Data.Date.UTC
  , toLocaleDateString
  , toLocaleString
  , toLocaleTimeString
  -- , year -- NOTE: Data.Date.UTC
  )
import Data.Date.UTC
  ( date
  , dateTime
  , dayOfMonth
  , dayOfWeek
  , hourOfDay
  , millisecondOfSecond
  , minuteOfHour
  , month
  , secondOfMinute
  , year
  )
import Data.Distributive
  ( Distributive
  , collect
  , cotraverse
  , distribute
  )
import Data.Either
  ( Either(Left, Right)
  , either
  , isLeft
  , isRight
  )
import Data.Either.Nested
  ( Either10()
  , Either2()
  , Either3()
  , Either4()
  , Either5()
  , Either6()
  , Either7()
  , Either8()
  , Either9()
  , either10
  , either10of10
  , either1of10
  , either1of2
  , either1of3
  , either1of4
  , either1of5
  , either1of6
  , either1of7
  , either1of8
  , either1of9
  , either2
  , either2of10
  , either2of2
  , either2of3
  , either2of4
  , either2of5
  , either2of6
  , either2of7
  , either2of8
  , either2of9
  , either3
  , either3of10
  , either3of3
  , either3of4
  , either3of5
  , either3of6
  , either3of7
  , either3of8
  , either3of9
  , either4
  , either4of10
  , either4of4
  , either4of5
  , either4of6
  , either4of7
  , either4of8
  , either4of9
  , either5
  , either5of10
  , either5of5
  , either5of6
  , either5of7
  , either5of8
  , either5of9
  , either6
  , either6of10
  , either6of6
  , either6of7
  , either6of8
  , either6of9
  , either7
  , either7of10
  , either7of7
  , either7of8
  , either7of9
  , either8
  , either8of10
  , either8of8
  , either8of9
  , either9
  , either9of10
  , either9of9
  )
import Data.Either.Unsafe
  ( fromLeft
  , fromRight
  )
import Data.Enum
  ( Cardinality(Cardinality)
  , Enum
  , cardinality
  , defaultFromEnum
  , defaultPred
  , defaultSucc
  , defaultToEnum
  , enumFromThenTo
  , enumFromTo
  , fromEnum
  , intFromTo
  , intStepFromTo
  , pred
  , runCardinality
  , succ
  , toEnum
  )
import Data.Equivalence
  ( Equivalence(Equivalence)
  , comparisonEquivalence
  , defaultEquivalence
  , runEquivalence
  )
import Data.Exists
  ( Exists()
  , mkExists
  , runExists
  )
import Data.FingerTree
  ( Digit()
  , FingerTree(Deep, Empty, Single)
  , LazySplit(LazySplit)
  , Node(Node2, Node3)
  , Split(Split)
  , ViewL(ConsL, NilL)
  , ViewR(NilR, SnocR)
  , app3
  -- , append -- NOTE: Prelude
  , compareFingerTree
  -- , cons -- TODO
  , consAll
  , deep
  , deepL
  , deepR
  , eqFingerTree
  -- , filter  -- TODO
  -- , fullyForce -- TODO
  -- , head -- TODO
  , headDigit
  -- , init -- TODO
  , initDigit
  -- , isEmpty -- TODO
  -- , last -- TODO
  , lastDigit
  , lazyEmpty
  , node2
  , node3
  , nodes
  , nodeToDigit
  -- , snoc -- TODO
  , snocAll
  -- , split -- NOTE: Data.String.Regex
  -- , tail -- TODO
  , tailDigit
  , toFingerTree
  , unfoldLeft
  , unfoldRight
  , unsafeSplitDigit
  , unsafeSplitTree
  , viewL
  , viewR
  )
import Data.Foldable
  ( Foldable
  , all
  , and
  , any
  , elem
  , find
  , fold
  , foldl
  , foldMap
  , foldr
  , for_
  , intercalate
  , mconcat
  , notElem
  , or
  , product
  , sequence_
  , sum
  , traverse_
  )
import Data.Foreign
  ( Foreign()
  , ForeignError(ErrorAtIndex, ErrorAtProperty, JSONError, TypeMismatch)
  , F()
  , isArray
  , isNull
  , isUndefined
  , parseJSON
  , readArray
  , readBoolean
  , readChar
  , readInt
  , readNumber
  , readString
  , tagOf
  , toForeign
  , typeOf
  , unsafeFromForeign
  , unsafeReadTagged
  )
import Data.Foreign.Class
  ( IsForeign
  , read
  , readJSON
  , readProp
  , readWith
  )
import Data.Foreign.Index
  ( Index
  , errorAt
  , hasOwnProperty
  , hasProperty
  -- , index -- TODO
  , ix
  , prop
  , (!)
  )
import Data.Foreign.Keys
  ( -- keys -- TODO
  )
import Data.Foreign.Null
  ( Null(Null)
  , readNull
  , runNull
  )
import Data.Foreign.NullOrUndefined
  ( NullOrUndefined(NullOrUndefined)
  , readNullOrUndefined
  , runNullOrUndefined
  )
import Data.Foreign.Undefined
  ( Undefined(Undefined)
  , readUndefined
  , runUndefined
  )
import Data.Function
  ( Fn0()
  , Fn1()
  , Fn10()
  , Fn2()
  , Fn3()
  , Fn4()
  , Fn5()
  , Fn6()
  , Fn7()
  , Fn8()
  , Fn9()
  , mkFn0
  , mkFn1
  , mkFn10
  , mkFn2
  , mkFn3
  , mkFn4
  , mkFn5
  , mkFn6
  , mkFn7
  , mkFn8
  , mkFn9
  , on
  , runFn0
  , runFn1
  , runFn10
  , runFn2
  , runFn3
  , runFn4
  , runFn5
  , runFn6
  , runFn7
  , runFn8
  , runFn9
  )
import Data.Functor
  ( (<$)
  , ($>)
  )
import Data.Functor.Contravariant
  ( Contravariant
  , cmap
  , (>#<)
  , (>$<)
  )
import Data.Functor.Contravariant.Divisible
  ( Decidable
  , Decide
  , Divide
  , Divisible
  , conquer
  , decide
  , decided
  , divide
  , divided
  , lose
  )
import Data.Functor.Coproduct
  ( Coproduct(Coproduct)
  , coproduct
  , left
  , right
  , runCoproduct
  )
import Data.Functor.Invariant
  ( Invariant
  , imap
  , imapF
  )
import Data.Generic
  ( Generic
  , GenericSignature(SigArray, SigBoolean, SigChar, SigInt, SigNumber, SigProd, SigRecord, SigString)
  , GenericSpine(SArray, SBoolean, SChar, SInt, SNumber, SProd, SRecord, SString)
  , Proxy(Proxy)
  , anyProxy
  , fromSpine
  , gCompare
  , gEq
  , gShow
  , isValidSpine
  , toSignature
  , toSpine
  )
import Data.Identity
  ( Identity(Identity)
  , runIdentity
  )
import Data.Inject
  ( Inject
  , inj
  , prj
  )
import Data.Int
  ( ceil
  , even
  , floor
  , fromNumber
  -- , fromString -- TODO
  , odd
  , round
  , toNumber
  )
import Data.Int.Bits
  ( complement
  , shl
  , shr
  , zshr
  , (.&.)
  , (.^.)
  , (.|.)
  )
import Data.Lazy
  ( -- Lazy() -- NOTE: Control.Lazy
  -- , defer -- NOTE: Control.Lazy
    force
  )
import Data.List
  ( List(Cons, Nil)
  -- , alterAt -- TODO
  -- , catMaybes -- TODO
  -- , concat -- TODO
  -- , concatMap -- TODO
  -- , delete -- TODO
  -- , deleteAt -- TODO
  -- , deleteBy -- TODO
  -- , drop -- TODO
  -- , dropWhile -- TODO
  -- , elemIndex -- TODO
  -- , elemLastIndex -- TODO
  -- , filter -- TODO
  -- , filterM -- TODO
  -- , findIndex -- TODO
  -- , findLastIndex -- TODO
  -- , foldM -- TODO
  -- , fromList -- TODO
  -- , group -- TODO
  -- , group' -- TODO
  -- , groupBy -- TODO
  -- , head -- TODO
  -- , index -- TODO
  -- , init -- TODO
  -- , insert -- TODO
  -- , insertAt -- TODO
  -- , insertBy -- TODO
  -- , intersect -- TODO
  -- , intersectBy -- TODO
  -- , last -- TODO
  -- , length -- TODO
  -- , many -- TODO
  -- , mapMaybe -- TODO
  -- , modifyAt -- TODO
  -- , nub -- TODO
  -- , nubBy -- TODO
  -- , null -- TODO
  -- , range -- TODO
  -- , replicate -- NOTE: Data.Unfoldable
  -- , replicateM -- TODO
  -- , reverse -- TODO
  -- , singleton -- NOTE: Data.Unfoldable
  -- , slice -- TODO
  -- , snoc -- TODO
  -- , some -- TODO
  -- , sort -- TODO
  -- , sortBy -- TODO
  -- , span -- TODO
  -- , tail -- TODO
  -- , take -- TODO
  -- , takeWhile -- TODO
  -- , toList -- TODO
  -- , uncons -- TODO
  -- , union -- TODO
  -- , unionBy -- TODO
  -- , unzip -- TODO
  -- , updateAt -- TODO
  -- , zip -- TODO
  -- , zipWith -- TODO
  -- , zipWithA -- TODO
  -- , (:) -- TODO
  -- , (!!) -- TODO
  -- , (..) -- TODO
  -- , (\\) -- TODO
  )
import Data.List.Lazy
  ( -- List(List) -- NOTE: Data.List
  -- , Step(Cons, NIl) -- NOTE: Data.List
  -- , alterAt -- TODO
  -- , catMaybes -- TODO
  -- , concat -- TODO
  -- , concatMap -- TODO
  -- , cons -- TODO
    cycle
  -- , delete -- TODO
  -- , deleteAt -- TODO
  -- , deleteBy -- TODO
  -- , drop -- TODO
  -- , dropWhile -- TODO
  -- , filter -- TODO
  -- , fromList -- TODO
  -- , group -- TODO
  -- , groupBy -- TODO
  -- , head -- TODO
  -- , index -- TODO
  -- , init -- TODO
  -- , insert -- TODO
  -- , insertAt -- TODO
  -- , insertBy -- TODO
  -- , intersect -- TODO
  -- , intersectBy -- TODO
  -- , iterate -- TODO
  -- , last -- TODO
  -- , length -- TODO
  -- , mapMaybe -- TODO
  -- , modifyAt -- TODO
  -- , nil -- TODO
  -- , nub -- TODO
  -- , nubBy -- TODO
  -- , null -- TODO
  -- , range -- TODO
  -- , repeat -- TODO
  -- , reverse -- TODO
  , runList
  -- , singleton -- NOTE: Data.Unfoldable
  -- , span -- TODO
  , step
  -- , tail -- TODO
  -- , take -- TODO
  -- , takeWhile -- TODO
  -- , toList -- TODO
  -- , uncons -- TODO
  -- , union -- TODO
  -- , unionBy -- TODO
  -- , updateAt -- TODO
  -- , zip -- TODO
  -- , zipWith -- TODO
  -- , (:) -- TODO
  -- , (!!) -- TODO
  -- , (..) -- TODO
  -- , (\\) -- TODO
  )
import Data.List.Unsafe
  ( -- head -- TODO
  -- , init -- TODO
  -- , last -- TODO
  -- , tail -- TODO
  )
import Data.List.ZipList
  ( ZipList(ZipList)
  , runZipList
  )
import Data.Map
  ( Map()
  -- , alter -- TODO
  -- , checkValid -- TODO
  -- , delete -- TODO
  -- , empty -- NOTE: Control.Plus
  -- , fromList -- TODO
  -- , fromListWith -- TODO
  -- , insert -- TODO
  -- , isEmpty -- TODO
  -- , keys -- TODO
  -- , lookup -- TODO
  -- , member -- TODO
  , showTree
  -- , singleton -- NOTE: Data.Unfoldable
  -- , size -- TODO
  -- , toList -- TODO
  -- , union -- TODO
  -- , unions -- TODO
  , unionWith
  -- , update -- TODO
  -- , values -- TODO
  )
import Data.Maybe
  ( Maybe(Nothing, Just)
  , fromMaybe
  , isJust
  , isNothing
  , maybe
  , maybe'
  )
import Data.Maybe.First
  ( First(First)
  , runFirst
  )
import Data.Maybe.Last
  ( Last(Last)
  , runLast
  )
import Data.Maybe.Unsafe
  ( fromJust
  , unsafeThrow
  )
import Data.Monoid
  ( Monoid
  , mempty
  )
import Data.Monoid.Additive
  ( Additive(Additive)
  , runAdditive
  )
import Data.Monoid.Conj
  ( Conj(Conj)
  , runConj
  )
import Data.Monoid.Disj
  ( Disj(Disj)
  , runDisj
  )
import Data.Monoid.Dual
  ( Dual(Dual)
  , runDual
  )
import Data.Monoid.Endo
  ( Endo(Endo)
  , runEndo
  )
import Data.Monoid.Multiplicative
  ( Multiplicative(Multiplicative)
  , runMultiplicative
  )
import Data.NaturalTransformation
  ( NaturalTransformation()
  , Natural()
  )
import Data.Nullable
  ( Nullable()
  , toMaybe
  , toNullable
  )
import Data.Op
  ( Op(Op)
  , runOp
  )
import Data.Predicate
  ( Predicate(Predicate)
  , runPredicate
  )
import Data.Profunctor
  ( Profunctor
  , arr
  -- , lmap -- NOTE: Data.Bifunctor
  -- , rmap -- NOTE: Data.Bifunctor
  )
import Data.Profunctor.Choice
  ( Choice
  , (+++)
  , (|||)
  )
import Data.Profunctor.Star
  ( Star(Star)
  , runStar
  )
import Data.Profunctor.Strong
  ( Strong
  , (***)
  , (&&&)
  )
import Data.Semiring.Free
  ( -- Free() -- NOTE: Control.Monad.Free
    free
  , liftFree
  , lowerFree
  -- , runFree -- NOTE: Control.Monad.Free
  )
import Data.Sequence
  ( Seq()
  , adjust
  -- , append -- NOTE: Prelude
  -- , concat -- TODO
  -- , concatMap -- TODO
  -- , cons -- TODO
  -- , drop -- TODO
  -- , empty -- TODO
  -- , filter -- TODO
  , fromSeq
  -- , fullyForce -- TODO
  -- , head -- TODO
  , inBounds
  -- , index -- TODO
  -- , init -- TODO
  -- , last -- TODO
  -- , length -- TODO
  -- , map -- NOTE: Prelude
  -- , null -- TODO
  -- , replace -- NOTE: Data.String.Regex
  -- , singleton -- NOTE: Data.Unfoldable
  -- , snoc -- TODO
  -- , sort -- TODO
  , splitAt
  -- , tail -- TODO
  -- , take -- TODO
  , toSeq
  -- , uncons -- TODO
  , unsnoc
  )
import Data.Sequence.NonEmpty
  ( -- Seq(Seq) -- NOTE: Data.Sequence
  -- , adjust -- NOTE: Data.Sequence
  -- , append -- NOTE: Prelude
  -- , cons -- TODO
  -- , drop -- TODO
  -- , filter -- TODO
  -- , fromSeq -- TODO
  -- , head -- TODO
  -- , inBounds -- NOTE: Data.Sequence
  -- , index -- TODO
  -- , init -- TODO
  -- , last -- TODO
  -- , length -- TODO
  -- , replace -- NOTE: Data.String.Regex
  -- , singleton -- NOTE: Data.Unfoldable
  -- , snoc -- TODO
  -- , splitAt -- NOTE: Data.Sequence
  -- , tail -- TODO
  -- , take -- TODO
    toPlain
  -- , uncons -- TODO
  -- , unsnoc -- NOTE: Data.Sequence
  )
import Data.Sequence.Ordered
  ( OrdSeq()
  , deleteAll
  -- , empty -- NOTE: Control.Plus
  , fromOrdSeq
  , fromOrdSeqDescending
  , greatest
  -- , insert -- TODO
  , intersection
  , least
  -- , length -- TODO
  , merge
  -- , null -- TODO
  , partition
  , popGreatest
  , popLeast
  -- , sort -- TODO
  , toOrdSeq
  )
import Data.Set
  ( Set()
  -- , checkValid -- TODO
  -- , delete -- TODO
  , difference
  -- , empty -- NOTE: Control.Plus
  -- , fromList -- TODO
  -- , insert -- TODO
  -- , isEmpty -- TODO
  -- , member -- TODO
  -- , singleton -- NOTE: Data.Unfoldable
  -- , size -- TODO
  -- , toList -- TODO
  -- , union -- TODO
  -- , unions -- TODO
  )
import Data.StrMap
  ( StrMap()
  -- , all -- NOTE: Data.Foldable
  -- , alter -- TODO
  -- , delete -- TODO
  -- , empty -- NOTE: Control.Plus
  -- , fold -- NOTE: Data.Foldable
  -- , foldM -- TODO
  -- , foldMap -- NOTE: Data.Foldable
  , foldMaybe
  , freezeST
  -- , fromList -- TODO
  -- , fromListWith -- TODO
  -- , insert -- TODO
  -- , isEmpty -- TODO
  , isSubmap
  -- , keys -- TODO
  -- , lookup -- TODO
  -- , member -- TODO
  -- , runST -- NOTE: Control.Monad.ST
  -- , singleton -- NOTE: Data.Unfoldable
  -- , size -- TODO
  , thawST
  -- , toList -- TODO
  -- , union -- TODO
  -- , unions -- TODO
  -- , update -- TODO
  -- , values -- TODO
  )
import Data.StrMap.ST
  ( STStrMap()
  -- , delete -- TODO
  , new
  -- , peek -- TODO
  , poke
  )
import Data.StrMap.ST.Unsafe
  ( unsafeGet
  )
import Data.StrMap.Unsafe
  ( -- unsafeIndex -- TODO
  )
import Data.String
  ( charAt
  , charCodeAt
  , contains
  , count
  -- , drop -- TODO
  -- , dropWhile -- TODO
  , fromChar
  , fromCharArray
  , indexOf
  , indexOf'
  , joinWith
  , lastIndexOf
  , lastIndexOf'
  -- , length -- TODO
  , localeCompare
  -- , null -- TODO
  -- , replace -- NOTE: Data.String.Regex
  -- , singleton -- NOTE: Data.Unfoldable
  -- , split -- NOTE: Data.String.Regex
  , stripPrefix
  , stripSuffix
  -- , take -- TODO
  -- , takeWhile -- TODO
  , toChar
  , toCharArray
  , toLower
  , toUpper
  , trim
  -- , uncons -- TODO
  )
import Data.String.Regex
  ( Regex()
  , RegexFlags()
  , flags
  , match
  , noFlags
  , parseFlags
  , regex
  , renderFlags
  , replace
  , replace'
  , search
  , source
  , split
  , test
  )
import Data.String.Unsafe
  ( char
  -- , charAt -- NOTE: Data.String
  -- , charCodeAt -- NOTE: Data.String
  )
import Data.These
  ( These(Both, That, This)
  , fromThese
  , thatOrBoth
  , these
  , theseLeft
  , theseRight
  , thisOrBoth
  )
import Data.Time
  ( HourOfDay(HourOfDay)
  , Hours(Hours)
  , MillisecondOfSecond(MillisecondOfSecond)
  , Milliseconds(Milliseconds)
  , MinuteOfHour(MinuteOfHour)
  , Minutes(Minutes)
  , SecondOfMinute(SecondOfMinute)
  , Seconds(Seconds)
  , TimeValue
  , fromHours
  , fromMilliseconds
  , fromMinutes
  , fromSeconds
  , toHours
  , toMilliseconds
  , toMinutes
  , toSeconds
  )
import Data.Traversable
  ( Accum()
  , Traversable
  , for
  , mapAccumL
  , mapAccumR
  , scanl
  , scanr
  , sequence
  , traverse
  )
import Data.Tuple
  ( Tuple(Tuple)
  , curry
  , fst
  -- , lookup -- TODO
  , snd
  , swap
  , uncurry
  )
import Data.Tuple.Nested
  ( Tuple10()
  , Tuple2()
  , Tuple3()
  , Tuple4()
  , Tuple5()
  , Tuple6()
  , Tuple7()
  , Tuple8()
  , Tuple9()
  , curry10
  , curry2
  , curry3
  , curry4
  , curry5
  , curry6
  , curry7
  , curry8
  , curry9
  , tuple10
  , tuple2
  , tuple3
  , tuple4
  , tuple5
  , tuple6
  , tuple7
  , tuple8
  , tuple9
  , uncurry10
  , uncurry2
  , uncurry3
  , uncurry4
  , uncurry5
  , uncurry6
  , uncurry7
  , uncurry8
  , uncurry9
  , (/\)
  )
import Data.Unfoldable
  ( Unfoldable
  , none
  , replicate
  , replicateA
  , singleton
  , unfoldr
  )
import Data.Validation
  ( V()
  , invalid
  , isValid
  , runV
  )
import Data.Validation.Semiring
  ( -- V() -- NOTE: Data.Validation
  -- , invalid -- NOTE: Data.Validation
  -- , isValid -- NOTE: Data.Validation
  -- , runV -- NOTE: Data.Validation
  )
import Data.Yoneda
  ( Yoneda(Yoneda)
  , runYoneda
  , liftYoneda
  , lowerYoneda
  )
import Global
  ( infinity
  , isFinite
  , isNaN
  , nan
  , readFloat
  -- , readInt -- NOTE: Data.Foreign
  )
import Global.Unsafe
  ( unsafeStringify
  )
import Math
  ( Radians()
  , abs
  , acos
  , asin
  , atan
  , atan2
  -- , ceil -- NOTE: Data.Int
  , cos
  , e
  , exp
  -- , floor -- NOTE: Data.Int
  , ln10
  , ln2
  , log
  , log10e
  , log2e
  , max
  , min
  , pi
  , pow
  -- , round -- NOTE: Data.Int
  , sin
  , sqrt
  , sqrt1_2
  , sqrt2
  , tan
  , (%)
  )
import Optic.Core
  ( (..)
  )
import Optic.Getter
  ( to
  , view
  , (^.)
  )
import Optic.Laws.Lens
  ( getSet
  , setGet
  , setSet
  , validLens
  )
import Optic.Lens
  ( lens
  , (??)
  )
import Optic.Prism
  ( clonePrism
  , is
  , isn't
  , matching
  , nearly
  , only
  , prism
  , prism'
  , withPrism
  )
import Optic.Setter
  ( argument
  , contramapped
  , mapped
  , over
  , set
  , set'
  , sets
  , (-~)
  , (?~)
  , (.~)
  , (*~)
  , (//~)
  , (&&~)
  , (%~)
  , (++~)
  , (+~)
  , (<>~)
  , (||~)
  )
import Optic.Types
  ( Accessing()
  , APrism()
  , APrismP()
  , ASetter()
  , ASetterP()
  , Getter()
  , Getting()
  , Lens()
  , LensP()
  , Optical()
  , OpticalP()
  , Prism()
  , PrismP()
  , Setter()
  , SetterP()
  , Setting()
  , SettingP()
  )
import Prelude
  ( Applicative
  , Apply
  , Bind
  , BooleanAlgebra
  , Bounded
  , BoundedOrd
  , Category
  , DivisionRing
  , Eq
  , Functor
  , ModuloSemiring
  , Monad
  , Num
  , Ord
  , Ordering(LT, EQ, GT)
  , Ring
  , Semigroup
  , Semigroupoid
  , Semiring
  , Show
  , Unit()
  , add
  , ap
  , append
  , apply
  , asTypeOf
  , bind
  , bottom
  , compare
  , compose
  , conj
  , const
  , disj
  , div
  , eq
  , flip
  , id
  , liftA1
  , liftM1
  , map
  , mod
  , mul
  , negate
  , not
  , one
  , otherwise
  , pure
  , return
  , show
  , sub
  , top
  , unit
  , unsafeCompare
  , void
  , zero
  , (-)
  , (*)
  , (/)
  , (/=)
  , (&&)
  , (#)
  , (+)
  , (++)
  , (<)
  , (<*>)
  , (<#>)
  , (<<<)
  , (<=)
  , (<>)
  , (<$>)
  , (==)
  , (>)
  , (>=)
  , (>>=)
  , (>>>)
  , (||)
  , ($)
  )
import Test.Assert
  ( ASSERT()
  , assert
  , assert'
  , assertThrows
  , assertThrows'
  )
import Type.Proxy
  ( -- Proxy(Proxy) -- NOTE: Data.Generic
    Proxy2(Proxy2)
  , Proxy3(Proxy3)
  )
import Unsafe.Coerce
  ( unsafeCoerce
  )
