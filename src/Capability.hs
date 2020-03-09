-- | A capability is a type class over a monad which specifies the effects that
-- a function is allowed to perform. Capabilities differ from traditional monad
-- transformer type classes in that they are completely independent of the way
-- the monad is constructed. A state capability can for instance be implemented
-- as a lens on a field in a larger state monad, or an error capability could
-- provide for throwing only a subset of the errors of an error monad.
--
-- This library defines several standard, reusable capabilities that replace the
-- mtl's monad-transformer type classes. Because capabilities are not tied to
-- a particular implementation of the monad, they cannot be discharged by
-- instance resolution. Instead this library provides combinators in the form of
-- newtypes with instances, to be used with deriving-via. To learn about
-- deriving via, watch Baldur Blondal's introductory video
-- <https://skillsmatter.com/skillscasts/10934-lightning-talk-stolen-instances-taste-just-fine>.
--
-- By way of comparison, with the mtl you would write something like
--
-- @
-- foo :: (MonadReader E, MonadState S) => a -> m ()
-- @
--
-- You can use @foo@ at type @a -> ReaderT E (State S)@. But you can't use @foo@
-- with the @ReaderT@ pattern
-- <https://www.fpcomplete.com/blog/2017/06/readert-design-pattern>. With this
-- library, you would instead have:
--
-- @
-- foo :: (HasReader "conf" E, HasState "st" S) => a -> m ()
-- @
--
-- Where @"conf"@ and @"st"@ are the names (also referred to as tags) of the
-- capabilities demanded by foo. Contrary to the mtl, capabilities are named,
-- rather than disambiguated by the type of their implied state, or exception.
-- This makes it easy to have multiple state capabilities.
--
-- To /provide/ these capabilities, for instance with the ReaderT pattern, do as
-- follows (for a longer form tutorial, check the
-- <https://github.com/tweag/capability#readme README>):
--
-- @
-- newtype MyM a = MyM (ReaderT (E, IORef s))
--   deriving (Functor, Applicative, Monad)
--   deriving (HasState "st" Int) via
--     ReaderIORef (Rename 2 (Pos 2 ()
--     (MonadReader (ReaderT (E, IORef s) IO))))
--   deriving (HasReader "conf" Int) via
--     (Rename 1 (Pos 1 ()
--     (MonadReader (ReaderT (E, IORef s) IO))))
-- @
--
-- Then you can use @foo@ at type @MyM@. Or any other type which can provide
-- these capabilites.
--
-- === Functional capabilities
--
-- When writing applications, as opposed to libraries, a capability /name/ often
-- determines its type parameters. It can be tiresome to write
--
-- @
-- f :: HasReader "config" Config m => …
-- @
--
-- over and over again.
--
-- To avoid this, each capability comes with a /functional/—here
-- @HasReader\'@—variant (in this terminology @HasReader@ is
-- /relational/). Where the type is deduced from the capability's name. The
-- mapping from name to type is done with the @'Capability.TypeOf.TypeOf'@
-- family, which is re-exported by every capability module.
--
-- @
-- type instance TypeOf Symbol "config" = Config
--
-- f :: HasReader' "config" m => …
-- @
--
-- == Module structure
--
-- Each module introduces a capability type class (or several related type
-- classes). Each class comes with a number of instances on newtypes (each
-- newtype should be seen as a combinator to be used with deriving-via to
-- provide the capability). Many newtypes come from the common
-- "Capability.Accessors" module (re-exported by each of the other modules),
-- which in particular contains a number of ways to address components of a data
-- type using the generic-lens library.
--
-- * "Capability.Reader" reader effects
-- * "Capability.State" state effects
-- * "Capability.Writer" writer effects
-- * "Capability.Error" throw and catch errors
-- * "Capability.Source" streaming in effect
-- * "Capability.Sink" streaming out effect (aka generators)
--
-- The effects are not all independent:
--
-- >     Source   Sink
-- >     /   \    /   \
-- >    /     \  /     \
-- > Reader   State    Writer
--
-- "Capability.Source" and "Capability.Sink" have just a method each, and no laws.
-- The bottom three, familiar from mtl, add methods and laws relating them.
-- The use of tags allows one to have independent effects that share a superclass.
-- E.g. @HasState "foo" Int@ and @HasWriter "bar" String@.
--
-- Some of the capability modules have a “discouraged” companion (such as
-- "Capability.Writer.Discouraged"). These modules contain deriving-via
-- combinators which you can use if you absolutely must: they are correct, but
-- inefficient, so we recommend that you do not.
--
-- Finally there is
--
-- * "Capability.Derive"
--
-- Which exports a (still experimental) 'Capability.Derive.derive' function,
-- which lets you run a computation which requires capabilities which are not
-- directly provided by the ambient monad, but can be derived from the
-- capabilities provided by the ambient monad.
--
-- == Further considerations
--
-- The tags of capabilities can be of any kind, they are not restricted to
-- symbols. When exporting functions demanding capabilities in libraries, it is
-- recommended to use a type as follows:
--
-- @
-- data Conf
--
-- foo :: HasReader Conf C => m ()
-- @
--
-- This way, @Conf@ can be qualified in case of a name conflict with another
-- library.

module Capability where
