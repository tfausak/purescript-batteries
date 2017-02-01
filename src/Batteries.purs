-- | 🔋 A PureScript prelude with more features.
module Batteries
  ( module Batteries
  , module Control.Alt
  , module Control.Alternative
  , module Control.Apply
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
  , module Control.Comonad.Trans.Class
  , module Control.Extend
  , module Control.Lazy
  , module Control.Monad.Aff
  , module Control.Monad.Aff.AVar
  , module Control.Monad.Aff.Class
  -- , module Control.Monad.Aff.Console
  , module Control.Monad.Aff.Unsafe
  , module Control.Monad.Cont.Class
  , module Control.Monad.Cont.Trans
  , module Control.Monad.Eff
  , module Control.Monad.Eff.Class
  , module Control.Monad.Eff.Console
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
  , module Control.Monad.Trans.Class
  , module Control.Monad.Writer
  , module Control.Monad.Writer.Class
  , module Control.Monad.Writer.Trans
  , module Control.MonadPlus
  , module Control.Parallel.Class
  , module Control.Plus
  , module Control.Semigroupoid
  -- , module Data.Array
  , module Data.Array.ST
  -- , module Data.Array.Unsafe
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
  , module Data.DateTime
  , module Data.DateTime.Instant
  , module Data.DateTime.Locale
  , module Data.Decide
  , module Data.Decidable
  , module Data.Distributive
  , module Data.Divide
  , module Data.Divisible
  , module Data.Either
  , module Data.Either.Nested
  , module Data.Enum
  , module Data.Equivalence
  , module Data.Exists
  -- , module Data.FingerTree
  , module Data.Foldable
  , module Data.Foreign
  , module Data.Foreign.Class
  , module Data.Foreign.Index
  -- , module Data.Foreign.Keys
  , module Data.Foreign.Null
  , module Data.Foreign.NullOrUndefined
  , module Data.Foreign.Undefined
  , module Data.Function
  , module Data.Function.Uncurried
  , module Data.Functor
  , module Data.Functor.Contravariant
  , module Data.Functor.Coproduct
  , module Data.Functor.Invariant
  , module Data.Generic
  , module Data.HeytingAlgebra
  , module Data.Identity
  , module Data.Inject
  , module Data.Int
  , module Data.Int.Bits
  , module Data.Lazy
  , module Data.List
  , module Data.List.Lazy
  -- , module Data.List.Unsafe
  , module Data.List.ZipList
  , module Data.Map
  , module Data.Maybe
  , module Data.Maybe.First
  , module Data.Maybe.Last
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
  , module Data.Ord
  , module Data.Ord.Down
  , module Data.Ord.Max
  , module Data.Ord.Min
  , module Data.Ord.Unsafe
  , module Data.Ordering
  , module Data.Predicate
  , module Data.Profunctor
  , module Data.Profunctor.Choice
  , module Data.Profunctor.Closed
  , module Data.Profunctor.Cochoice
  , module Data.Profunctor.Costar
  , module Data.Profunctor.Costrong
  , module Data.Profunctor.Star
  , module Data.Profunctor.Strong
  , module Data.Semiring.Free
  -- , module Data.Sequence
  -- , module Data.Sequence.NonEmpty
  -- , module Data.Sequence.Ordered
  , module Data.Set
  , module Data.StrMap
  , module Data.StrMap.ST
  , module Data.StrMap.ST.Unsafe
  -- , module Data.StrMap.Unsafe
  , module Data.String
  , module Data.String.Regex
  , module Data.String.Unsafe
  , module Data.These
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Data.Unfoldable
  , module Data.Validation.Semigroup
  -- , module Data.Validation.Semiring
  , module Data.Yoneda
  , module Debug.Trace
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
  ( class Alt
  , alt
  , (<|>)
  )
import Control.Alternative
  ( class Alternative
  )
import Control.Apply
  ( applyFirst
  , applySecond
  , lift2
  , lift3
  , lift4
  , lift5
  )
import Control.Biapplicative
  ( class Biapplicative
  )
import Control.Biapply
  ( class Biapply
  , bilift2
  , bilift3
  , (*>>)
  , (<<*)
  , (<<*>>)
  , (<<$>>)
  )
import Control.Bind
  ( bindFlipped
  , composeKleisli
  , composeKleisliFlipped
  , ifM
  , join
  )
import Control.Comonad
  ( class Comonad
  , extract
  )
import Control.Comonad.Cofree
  ( Cofree
  -- , head -- TODO
  , mkCofree
  -- , tail -- TODO
  )
import Control.Comonad.Env
  ( Env
  , env
  , mapEnv
  , runEnv
  , withEnv
  )
import Control.Comonad.Env.Class
  ( class ComonadEnv
  -- , ask -- TODO
  -- , asks -- TODO
  -- , local -- TODO
  )
import Control.Comonad.Env.Trans
  ( EnvT(EnvT)
  , mapEnvT
  , runEnvT
  , withEnvT
  )
import Control.Comonad.Store
  ( Store
  , runStore
  , store
  )
import Control.Comonad.Store.Class
  ( class ComonadStore
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
  ( Traced
  , runTraced
  , traced
  )
import Control.Comonad.Traced.Class
  ( class ComonadTraced
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
import Control.Comonad.Trans.Class
  ( class ComonadTrans
  , lower
  )
import Control.Extend
  ( class Extend
  , duplicate
  , extend
  , (<<=)
  , (=<=)
  , (=>=)
  , (=>>)
  )
import Control.Lazy
  ( class Lazy
  , defer
  , fix
  )
import Control.Monad.Aff
  ( Aff
  , Canceler(Canceler)
  , PureAff
  , apathize
  , attempt
  , cancel
  , cancelWith
  , finally
  , forkAff
  , forkAll
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
  ( AffAVar
  , AVAR
  , AVar
  , killVar
  , makeVar
  , makeVar'
  , modifyVar
  , putVar
  , takeVar
  )
import Control.Monad.Aff.Class
  ( class MonadAff
  , liftAff
  )
-- import Control.Monad.Aff.Console -- NOTE: Control.Monad.Eff.Console
import Control.Monad.Aff.Unsafe
  ( unsafeCoerceAff
  )
import Control.Monad.Cont.Class
  ( class MonadCont
  , callCC
  )
import Control.Monad.Cont.Trans
  ( ContT(ContT)
  , mapContT
  , runContT
  , withContT
  )
import Control.Monad.Eff
  ( Eff
  , Pure
  , forE
  , foreachE
  , runPure
  , untilE
  , whileE
  )
import Control.Monad.Eff.Class
  ( class MonadEff
  , liftEff
  )
import Control.Monad.Eff.Console
  ( CONSOLE
  , errorShow
  , infoShow
  , logShow
  , warnShow
  )
import Control.Monad.Eff.Exception
  ( EXCEPTION
  , Error
  , catchException
  , error
  , message
  , stack
  , throwException
  )
import Control.Monad.Eff.Random
  ( RANDOM
  , random
  , randomBool
  , randomInt
  , randomRange
  )
import Control.Monad.Eff.Ref
  ( REF
  , Ref
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
  ( unsafePerformEff
  , unsafeCoerceEff
  )
import Control.Monad.Error.Class
  ( class MonadError
  , catchError
  , catchJust
  , throwError
  )
import Control.Monad.Except
  ( Except
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
  ( Free
  , hoistFree
  , foldFree
  , injF
  , liftF
  , liftFI
  , runFree
  , runFreeM
  , suspendF
  )
import Control.Monad.List.Trans
  ( ListT
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
  ( RWS
  , evalRWS
  , execRWS
  , mapRWS
  , runRWS
  , rws
  , withRWS
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
  ( Reader
  , mapReader
  , runReader
  , withReader
  )
import Control.Monad.Reader.Class
  ( class MonadReader
  , class MonadAsk
  , ask
  , asks
  , local
  )
import Control.Monad.Reader.Trans
  ( ReaderT(ReaderT)
  , mapReaderT
  , runReaderT
  , withReaderT
  )
import Control.Monad.Rec.Class
  ( class MonadRec
  , forever
  , tailRec
  , tailRecM
  , tailRecM2
  , tailRecM3
  )
import Control.Monad.ST
  ( ST
  , STRef
  , modifySTRef
  , newSTRef
  , pureST
  , readSTRef
  , runST
  , writeSTRef
  )
import Control.Monad.State
  ( State
  , evalState
  , execState
  , mapState
  , runState
  , withState
  )
import Control.Monad.State.Class
  ( class MonadState
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
  ( Trampoline
  , delay
  , delay'
  , done
  , runTrampoline
  , suspend
  )
import Control.Monad.Trans.Class
  ( class MonadTrans
  , lift
  )
import Control.Monad.Writer
  ( Writer
  , execWriter
  , mapWriter
  , runWriter
  )
import Control.Monad.Writer.Class
  ( class MonadWriter
  , class MonadTell
  , tell
  , censor
  , listen
  , pass
  , listens
  )
import Control.Monad.Writer.Trans
  ( WriterT(WriterT)
  , execWriterT
  , mapWriterT
  , runWriterT
  )
import Control.MonadPlus
  ( class MonadPlus
  , guard
  )
import Control.Parallel.Class
  ( class Parallel
  , parallel
  , sequential
  , ParCont(..)
  )
import Control.Plus
  ( class Plus
  , empty
  )
import Control.Semigroupoid
  ( composeFlipped
  )
-- import Data.Array -- TODO
import Data.Array.ST
  ( Assoc
  , STArray
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
-- import Data.Array.Unsafe -- TODO
import Data.Bifoldable
  ( class Bifoldable
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
  ( class Bifunctor
  , lmap
  , rmap
  )
import Data.Bifunctor.Clown
  ( Clown(Clown)
  )
import Data.Bifunctor.Flip
  ( Flip(Flip)
  )
import Data.Bifunctor.Join
  ( Join(Join)
  )
import Data.Bifunctor.Joker
  ( Joker(Joker)
  )
import Data.Bifunctor.Product
  ( Product(Product)
  )
import Data.Bifunctor.Wrap
  ( Wrap(Wrap)
  )
import Data.Bitraversable
  ( class Bitraversable
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
  -- , toUpper -- NOTE: Data.String
  )
import Data.Comparison
  ( Comparison(Comparison)
  , defaultComparison
  )
import Data.Const
  ( Const(Const)
  )
import Data.Coyoneda
  ( Coyoneda(Coyoneda)
  , CoyonedaF
  , coyoneda
  , hoistCoyoneda
  , liftCoyoneda
  , lowerCoyoneda
  )
import Data.DateTime
  ( Date
  , DateTime(DateTime)
  , Day
  , Hour
  , Millisecond
  , Minute
  , Month(April, August, December, February, January, July, June, March, May, November, October, September)
  , Second
  , Time(Time)
  , Weekday(Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday)
  , Year
  -- , adjust -- NOTE: Data.Sequence
  , canonicalDate
  , date
  , day
  , diff
  , exactDate
  , hour
  , millisecond
  , minute
  , month
  -- , second -- NOTE: Data.Profunctor.Strong
  , setHour
  , setMillisecond
  , setMinute
  , setSecond
  , time
  , weekday
  , year
  )
import Data.DateTime.Instant
  ( Instant
  , fromDateTime
  , instant
  , toDateTime
  , unInstant
  )
import Data.DateTime.Locale
  ( LocalDate
  , LocalDateTime
  , LocalTime
  , LocalValue(LocalValue)
  , Locale(Locale)
  , LocaleName(LocaleName)
  )
import Data.Decide
  ( class Decide
  , choose
  , chosen
  )
import Data.Decidable
  ( class Decidable
  , lose
  , lost
  )
import Data.Distributive
  ( class Distributive
  , collect
  , cotraverse
  , distribute
  )
import Data.Divide
  ( class Divide
  , divide
  , divided
  )
import Data.Divisible
  ( class Divisible
  , conquer
  )
import Data.Either
  ( Either(Left, Right)
  , either
  , isLeft
  , isRight
  )
import Data.Either.Nested
  ( in1, in2, in3, in4, in5, in6, in7, in8, in9, in10
  , at1, at2, at3, at4, at5, at6, at7, at8, at9, at10
  , Either1, Either2, Either3, Either4, Either5, Either6, Either7, Either8, Either9, Either10
  , either1, either2, either3, either4, either5, either6, either7, either8, either9, either10
  , E2, E3, E4, E5, E6, E7, E8, E9, E10, E11
  )
import Data.Enum
  ( class Enum, succ, pred
  , defaultSucc
  , defaultPred
  , enumFromTo
  , enumFromThenTo
  , upFrom
  , downFrom
  , Cardinality(..)
  , class BoundedEnum, cardinality, toEnum, fromEnum, toEnumWithDefaults
  , defaultCardinality
  , defaultToEnum
  , defaultFromEnum
  )
import Data.Equivalence
  ( Equivalence(Equivalence)
  , comparisonEquivalence
  , defaultEquivalence
  )
import Data.Exists
  ( Exists
  , mkExists
  , runExists
  )
-- import Data.FingerTree -- FIXME: update upstream
  -- ( Digit
  -- , FingerTree(Deep, Empty, Single)
  -- , LazySplit(LazySplit)
  -- , Node(Node2, Node3)
  -- , Split(Split)
  -- , ViewL(ConsL, NilL)
  -- , ViewR(NilR, SnocR)
  -- , app3
  -- , append -- NOTE: Prelude
  -- , compareFingerTree
  -- , cons -- TODO
  -- , consAll
  -- , deep
  -- , deepL
  -- , deepR
  -- , eqFingerTree
  -- , filter  -- TODO
  -- , fullyForce -- TODO
  -- , head -- TODO
  -- , headDigit
  -- , init -- TODO
  -- , initDigit
  -- , isEmpty -- TODO
  -- , last -- TODO
  -- , lastDigit
  -- , lazyEmpty
  -- , node2
  -- , node3
  -- , nodes
  -- , nodeToDigit
  -- , snoc -- TODO
  -- , snocAll
  -- , split -- NOTE: Data.String.Regex
  -- , tail -- TODO
  -- , tailDigit
  -- , toFingerTree
  -- , unfoldLeft
  -- , unfoldRight
  -- , unsafeSplitDigit
  -- , unsafeSplitTree
  -- , viewL
  -- , viewR
  -- )
import Data.Foldable
  ( class Foldable
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
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , notElem
  , or
  , product
  , sequence_
  , sum
  , traverse_
  )
import Data.Foreign
  ( Foreign
  , ForeignError(ErrorAtIndex, ErrorAtProperty, JSONError, TypeMismatch)
  , F
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
  ( class IsForeign
  , read
  , readJSON
  , readProp
  , readWith
  )
import Data.Foreign.Index
  ( class Index
  , errorAt
  , hasOwnProperty
  , hasProperty
  -- , index -- TODO
  , ix
  , prop
  , (!)
  )
-- import Data.Foreign.Keys -- TODO
import Data.Foreign.Null
  ( Null(Null)
  , readNull
  , unNull
  )
import Data.Foreign.NullOrUndefined
  ( NullOrUndefined(NullOrUndefined)
  , readNullOrUndefined
  , unNullOrUndefined
  )
import Data.Foreign.Undefined
  ( Undefined(Undefined)
  , readUndefined
  , unUndefined
  )
import Data.Function
  ( applyFlipped
  , on
  )
import Data.Function.Uncurried
  ( Fn0
  , Fn1
  , Fn10
  , Fn2
  , Fn3
  , Fn4
  , Fn5
  , Fn6
  , Fn7
  , Fn8
  , Fn9
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
  ( mapFlipped
  , voidLeft
  , voidRight
  )
import Data.Functor.Contravariant
  ( class Contravariant
  , cmap
  , (>#<)
  , (>$<)
  )
import Data.Functor.Coproduct
  ( Coproduct(Coproduct)
  , coproduct
  , left
  , right
  )
import Data.Functor.Invariant
  ( class Invariant
  , imap
  , imapF
  )
import Data.Generic
  ( class Generic
  , GenericSignature(SigArray, SigBoolean, SigChar, SigInt, SigNumber, SigProd, SigRecord, SigString)
  , GenericSpine(SArray, SBoolean, SChar, SInt, SNumber, SProd, SRecord, SString)
  , fromSpine
  , gCompare
  , gEq
  , gShow
  , isValidSpine
  , toSignature
  , toSpine
  )
import Data.HeytingAlgebra
  ( ff
  , implies
  , tt
  )
import Data.Identity
  ( Identity(Identity)
  )
import Data.Inject
  ( class Inject
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
  ( -- Lazy -- NOTE: Control.Lazy
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
-- import Data.List.Unsafe -- TODO
import Data.List.ZipList
  ( ZipList(ZipList)
  )
import Data.Map
  ( Map
  -- , alter -- TODO
  -- , checkValid -- TODO
  -- , delete -- TODO
  -- , empty -- NOTE: Control.Plus
  -- , fromFoldable -- TODO
  -- , fromFoldableWith -- TODO
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
  )
import Data.Maybe.Last
  ( Last(Last)
  )
import Data.Monoid
  ( class Monoid
  , mempty
  )
import Data.Monoid.Additive
  ( Additive(Additive)
  )
import Data.Monoid.Conj
  ( Conj(Conj)
  )
import Data.Monoid.Disj
  ( Disj(Disj)
  )
import Data.Monoid.Dual
  ( Dual(Dual)
  )
import Data.Monoid.Endo
  ( Endo(Endo)
  )
import Data.Monoid.Multiplicative
  ( Multiplicative(Multiplicative)
  )
import Data.NaturalTransformation
  ( NaturalTransformation
  )
import Data.Nullable
  ( Nullable
  , toMaybe
  , toNullable
  )
import Data.Op
  ( Op(Op)
  )
import Data.Ord
  ( abs
  , greaterThan
  , greaterThanOrEq
  , lessThan
  , lessThanOrEq
  , signum
  )
import Data.Ordering
  ( invert
  )
import Data.Ord.Unsafe
  ( unsafeCompare
  )
import Data.Ord.Down
  ( Down(Down)
  )
import Data.Ord.Max
  ( Max(Max)
  )
import Data.Ord.Min
  ( Min(Min)
  )
import Data.Predicate
  ( Predicate(Predicate)
  )
import Data.Profunctor
  ( class Profunctor
  , arr
  , dimap
  -- , lmap -- NOTE: Data.Bifunctor
  -- , rmap -- NOTE: Data.Bifunctor
  )
import Data.Profunctor.Choice
  ( class Choice
  , fanin
  -- , left -- NOTE: Data.Functor.Coproduct
  -- , right -- NOTE: Data.Functor.Coproduct
  , splitChoice
  , (+++)
  , (|||)
  )
import Data.Profunctor.Closed
  ( class Closed
  , closed
  )
import Data.Profunctor.Cochoice
  ( class Cochoice
  , unleft
  , unright
  )
import Data.Profunctor.Costar
  ( Costar(Costar)
  )
import Data.Profunctor.Costrong
  ( class Costrong
  , unfirst
  , unsecond
  )
import Data.Profunctor.Star
  ( Star(Star)
  )
import Data.Profunctor.Strong
  ( class Strong
  , fanout
  , first
  , second
  , splitStrong
  , (***)
  , (&&&)
  )
import Data.Semiring.Free
  ( -- Free -- NOTE: Control.Monad.Free
    free
  , liftFree
  , lowerFree
  -- , runFree -- NOTE: Control.Monad.Free
  )
-- import Data.Sequence -- FIXME: update upstream
  -- ( Seq
  -- , adjust
  -- , append -- NOTE: Prelude
  -- , concat -- TODO
  -- , concatMap -- TODO
  -- , cons -- TODO
  -- , drop -- TODO
  -- , empty -- TODO
  -- , filter -- TODO
  -- , fullyForce -- TODO
  -- , head -- TODO
  -- , inBounds
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
  -- , splitAt
  -- , tail -- TODO
  -- , take -- TODO
  -- , uncons -- TODO
  -- , unsnoc
  -- )
-- import Data.Sequence.NonEmpty -- FIXME: update upstream
  -- ( -- Seq(Seq) -- NOTE: Data.Sequence
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
  -- toPlain
  -- , uncons -- TODO
  -- , unsnoc -- NOTE: Data.Sequence
  -- )
-- import Data.Sequence.Ordered -- FIXME: update upstream
  -- ( OrdSeq
  -- , deleteAll
  -- , empty -- NOTE: Control.Plus
  -- , greatest
  -- , insert -- TODO
  -- , intersection -- TODO: Data.Set
  -- , least
  -- , length -- TODO
  -- , merge
  -- , null -- TODO
  -- , partition
  -- , popGreatest
  -- , popLeast
  -- , sort -- TODO
  -- )
import Data.Set
  ( Set
  -- , checkValid -- TODO
  -- , delete -- TODO
  , difference
  -- , empty -- NOTE: Control.Plus
  -- , fromList -- TODO
  -- , insert -- TODO
  , intersection
  -- , isEmpty -- TODO
  -- , member -- TODO
  , properSubset
  -- , singleton -- NOTE: Data.Unfoldable
  -- , size -- TODO
  , subset
  -- , toList -- TODO
  -- , union -- TODO
  -- , unions -- TODO
  )
import Data.StrMap
  ( StrMap
  -- , all -- NOTE: Data.Foldable
  -- , alter -- TODO
  -- , delete -- TODO
  -- , empty -- NOTE: Control.Plus
  -- , fold -- NOTE: Data.Foldable
  -- , foldM -- TODO
  -- , foldMap -- NOTE: Data.Foldable
  , foldMaybe
  , freezeST
  -- , fromFoldable -- TODO
  -- , fromFoldableWith -- TODO
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
  ( STStrMap
  -- , delete -- TODO
  , new
  -- , peek -- TODO
  , poke
  )
import Data.StrMap.ST.Unsafe
  ( unsafeGet
  )
-- import Data.StrMap.Unsafe -- TODO
import Data.String
  ( charAt
  , charCodeAt
  , contains
  -- , drop -- TODO
  -- , dropWhile -- TODO
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
  ( Regex
  , regex
  , source
  , flags
  , renderFlags
  , parseFlags
  , test
  , match
  , replace
  , replace'
  , search
  , split
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
import Data.Traversable
  ( class Traversable
  , Accum
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
  ( Tuple10
  , Tuple2
  , Tuple3
  , Tuple4
  , Tuple5
  , Tuple6
  , Tuple7
  , Tuple8
  , Tuple9
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
  ( class Unfoldable
  , none
  , replicate
  , replicateA
  , singleton
  , unfoldr
  )
import Data.Validation.Semigroup
  ( V
  , invalid
  , isValid
  , unV
  )
-- import Data.Validation.Semiring -- NOTE: Data.Validation.Semigroup
import Data.Yoneda
  ( Yoneda(Yoneda)
  , liftYoneda
  , lowerYoneda
  , runYoneda
  )
import Debug.Trace
  ( spy
  , trace
  , traceA
  , traceAny
  , traceAnyA
  , traceAnyM
  , traceShow
  , traceShowA
  , traceShowM
  )
import Global
  ( decodeURI
  , decodeURIComponent
  , encodeURI
  , encodeURIComponent
  , infinity
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
  ( Radians
  -- , abs -- NOTE: Prelude
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
  -- , max -- NOTE: Prelude
  -- , min -- NOTE: Prelude
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
  , weiv
  , (^.)
  )
import Optic.Laws.Lens
  ( getSet
  , setGet
  , setSet
  , validLens
  )
import Optic.Lens
  ( flip'
  , lens
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
  -- ( add -- NOTE: Prelude
  -- , and -- NOTE: Prelude
  ( argument
  -- , concat -- NOTE: Data.List
  , contramapped
  -- , div -- NOTE: Prelude
  , mapped
  -- , mul -- NOTE: Prelude
  -- , or -- NOTE: Prelude
  , over
  , set
  , set'
  , setJust
  , sets
  -- , sub -- NOTE: Prelude
  , (-~)
  , (?~)
  , (.~)
  , (*~)
  , (/~)
  , (&&~)
  , (%~)
  , (+~)
  , (<>~)
  , (||~)
  )
import Optic.Types
  ( Accessing
  , APrism
  , APrism'
  , ASetter
  , ASetter'
  , Getter
  , Getting
  , Lens
  , Lens'
  , Optical
  , Optical'
  , Prism
  , Prism'
  , Setter
  , Setter'
  , Setting
  , Setting'
  )
import Prelude
  ( class Applicative
  , class Apply
  , class Bind
  , class BooleanAlgebra
  , class Bounded
  , class Category
  , class CommutativeRing
  , class Eq
  , class EuclideanRing
  , class Field
  , class Functor
  , class HeytingAlgebra
  , class Monad
  , class Ord
  , class Ring
  , class Semigroup
  , class Semigroupoid
  , class Semiring
  , class Show
  , Ordering(LT, EQ, GT)
  , Unit
  , Void
  , absurd
  , add
  , ap
  , append
  , apply
  , between
  , bind
  , bottom
  , clamp
  , compare
  , comparing
  , compose
  , conj
  , const
  , degree
  , disj
  , div
  , eq
  , flip
  , id
  , liftA1
  , liftM1
  , map
  , max
  , min
  , mod
  , mul
  , negate
  , not
  , notEq
  , one
  , otherwise
  , pure
  , show
  , sub
  , top
  , unit
  , unless
  , void
  , zero
  , type (~>)
  , (-)
  , (*)
  , (*>)
  , (/)
  , (/=)
  , (&&)
  , (#)
  , (+)
  , (<)
  , (<*)
  , (<*>)
  , (<#>)
  , (<<<)
  , (<=)
  , (<=<)
  , (<>)
  , (<$)
  , (<$>)
  , (=<<)
  , (==)
  , (>)
  , (>=)
  , (>=>)
  , (>>=)
  , (>>>)
  , (||)
  , ($)
  , ($>)
  )
import Test.Assert
  ( ASSERT
  , assert
  , assert'
  , assertThrows
  , assertThrows'
  )
import Type.Proxy
  ( Proxy(Proxy)
  , Proxy2(Proxy2)
  , Proxy3(Proxy3)
  )
import Unsafe.Coerce
  ( unsafeCoerce
  )
