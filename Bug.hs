{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Bug where

import Data.Functor.Identity
import Data.Proxy
import Control.Applicative (Const(..))


data Expr a = Expr !Int String

-- Stubs
data UTCTime = UTCTime
data NoteType = NoteType
data EventType = EventType
data MetaData = MetaData

newtype UserId = UserId Int
newtype EventId = EventId Int

newtype (:->) s a = Col { getCol :: a }

data Nat = Z | S !Nat

type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = 'Z
  RIndex r (s ': rs) = 'S (RIndex r rs)

type family RImage (rs :: [k]) (ss :: [k]) :: [Nat] where
  RImage '[] ss = '[]
  RImage (r ': rs) ss = RIndex r ss ': RImage rs ss

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)


data Rec :: [*] -> * where
  RNil :: Rec '[]
  (:&) :: !r -> !(Rec rs) -> Rec (r ': rs)

class i ~ RIndex r rs => RElem (r :: *) (rs :: [*]) (i :: Nat) where
  rlens :: Functor g
        => sing r
        -> (r -> g r)
        -> Rec rs
        -> g (Rec rs)

  rget :: sing r -> Rec rs -> r
  rget k = getConst . rlens k Const
  rput :: r
       -> Rec rs
       -> Rec rs
  rput y = runIdentity . rlens Proxy (\_ -> Identity y)

instance RElem r (r ': rs) 'Z where
  rlens _ f (x :& xs) = fmap (:& xs) (f x)
  {-# INLINE rlens #-}

instance (RIndex r (s ': rs) ~ 'S i, RElem r rs i) => RElem r (s ': rs) ('S i) where
  rlens p f (x :& xs) = fmap (x :&) (rlens p f xs)
  {-# INLINE rlens #-}

lens :: Functor f
     => (t -> s)
     -> (t -> a -> b)
     -> (s -> f a)
     -> t
     -> f b
lens sa sbt afb s = fmap (sbt s) $ afb (sa s)
{-# INLINE lens #-}


type IElem r rs = RElem r rs (RIndex r rs)

class is ~ RImage rs ss => RSubset (rs :: [*]) (ss :: [*]) is where

  -- | This is a lens into a slice of the larger record. Morally, we have:
  --
  -- > rsubset :: Lens' (Rec f ss) (Rec f rs)
  rsubset
    :: Functor g
    => (Rec rs -> g (Rec rs))
    -> Rec ss
    -> g (Rec ss)

  -- | The getter of the 'rsubset' lens is 'rcast', which takes a larger record
  -- to a smaller one by forgetting fields.
  rcast
    :: Rec ss
    -> Rec rs
  rcast = getConst . rsubset Const
  {-# INLINE rcast #-}

  -- | The setter of the 'rsubset' lens is 'rreplace', which allows a slice of
  -- a record to be replaced with different values.
  rreplace
    :: Rec rs
    -> Rec ss
    -> Rec ss
  rreplace rs = runIdentity . rsubset (\_ -> Identity rs)
  {-# INLINE rreplace #-}

instance RSubset '[] ss '[] where
  rsubset = lens (const RNil) const

instance (RElem r ss i , RSubset rs ss is) => RSubset (r ': rs) ss (i ': is) where
  rsubset = lens (\ss -> rget Proxy ss :& rcast ss) set
    where
      set :: Rec ss -> Rec (r ': rs) -> Rec ss
      set ss (r :& rs) = rput r $ rreplace rs ss

type ISubset rs ss = RSubset rs ss (RImage rs ss)


type JournalEventColumns = '[ "id"             :-> Expr EventId
                            , "event_type"     :-> Expr EventType
                            , "target_id"      :-> Expr Int
                            , "target_type"    :-> Expr String
                            , "created_at"     :-> Expr UTCTime
                            , "created_by"     :-> Expr UserId
                            ]

type JournalNoteColumns = '[ "id"             :-> Expr EventId
                           , "event_type"     :-> Expr EventType
                           , "note_type"      :-> Expr NoteType
                           , "parent_id"      :-> Expr (Maybe EventId)
                           --, "modified_at"    :-> Expr UTCTime
                           --, "modified_by"    :-> Expr UserId
                           ]

type JournalExpandedColumns = '[ "id"             :-> Expr EventId
                               , "target_id"      :-> Expr Int
                               , "target_type"    :-> Expr String
                               , "event_type"     :-> Expr EventType
                               , "note_type"      :-> Expr NoteType
                               , "parent_id"      :-> Expr (Maybe EventId)
                               , "created_at"     :-> Expr UTCTime
                               , "created_by"     :-> Expr UserId
                               --, "modified_at"    :-> Expr UTCTime
                               --, "modified_by"    :-> Expr UserId
                               ]

join_note :: Rec (JournalEventColumns ++ JournalNoteColumns)
             -> Rec JournalExpandedColumns
join_note = rcast
