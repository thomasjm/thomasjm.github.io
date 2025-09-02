
# type-families-typescript

This is a demonstration of how to map a certain cool API design pattern from Haskell to a TypeScript client library.

## The Setup

Suppose you want to write a service that deals with two kinds of messages: "requests" and "notifications." Requests have an ID attached and require a response containing the same ID, whereas notifications don't require a response. Messages can be sent from either the client or server.

This is a nice general framework for a service and is how the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) is designed.

## The Haskell pattern

What's the best way to represent this service in Haskell types? The following pattern comes from the [lsp-types](https://github.com/alanz/lsp/tree/master/lsp-types) library, which helps power [haskell-language-server](https://github.com/haskell/haskell-language-server/).

Let's suppose we want our API to support two client-to-server messages: one a request called `Login` and one a notification called `ReportClick`. We write out our data types like this:

``` haskell
data From = FromServer | FromClient
data MethodType = Notification | Request

data Method (f :: From) (t :: MethodType) where
  Login :: Method 'FromClient 'Request
  ReportClick :: Method 'FromClient 'Notification
```

Here we're using `DataKinds` and `KindSignatures` to tag the different constructors with information about where they come from and what type of message they are.

Now let's write our generic message constructors:

``` haskell
-- | A request
data RequestMessage (m :: Method f 'Request) =
  RequestMessage {
    _id :: T.Text
    , _method :: SMethod m
    , _params :: MessageParams m
    } deriving Generic

-- | A notification
data NotificationMessage (m :: Method f 'Notification) =
  NotificationMessage {
    _method  :: SMethod m
    , _params  :: MessageParams m
    } deriving Generic

-- | A response to a request
data ResponseMessage (m :: Method f 'Request) =
  ResponseMessage
    { _id :: Maybe T.Text
    , _result :: Either String (ResponseResult m)
    } deriving Generic
```

As you can see, a `RequestMessage` has an `id` while a `NotificationMessage` does not. A `ResponseMessage` contains a `result`, which can be either a failure or a successful value. Each message is parameterized by a version of `Method`. (Don't worry about `SMethod`, it's just a counterpart to `Method` that's easier to use at the term level.)

Notice how these message constructors define their `params` and `result` in terms of a type family call. Let's write those type families now:

``` haskell
type family MessageParams (m :: Method f t) :: Kind.Type where
  MessageParams 'Login = LoginParams
  MessageParams 'ReportClick = ReportClickParams

type family ResponseResult (m :: Method f 'Request) :: Kind.Type where
  ResponseResult 'Login = LoginResult
```

Now we can define the parameter and result types for each method!

I won't go in detail about why this is great, but you can look at `haskell-language-server` to see how this setup adds a lot of type safety to your server, making sure you return the right response type to each message, etc.

## Mapping it to TypeScript

Now the question is, how can I generate a TypeScript client library for my service that has the same amount of type safety?

If you're familiar with `aeson-typescript`, you know you can use it to generate TypeScript representations of Haskell data types. For example, I could emit types for all of the input and output types:

``` haskell
import Data.Aeson.TypeScript.TH

data LoginParams = LoginParams { username :: T.Text, password :: T.Text }
$(deriveJSONAndTypeScript A.defaultOptions ''LoginParams)

data ReportClickParams = ReportClickParams { ... }
$(deriveJSONAndTypeScript A.defaultOptions ''ReportClickParams)

data LoginResult = LoginResult { profilePicture :: T.Text }
$(deriveJSONAndTypeScript A.defaultOptions ''LoginResult)

main = do
  putStrLn $ formatTSDeclarations $ (
    (getTypeScriptDeclaration (Proxy :: Proxy LoginParams))
    <> (getTypeScriptDeclaration (Proxy :: Proxy ReportClickParams))
    <> (getTypeScriptDeclaration (Proxy :: Proxy LoginResult))
  )
```

This will output some TypeScript interfaces for these types, suitable for putting in a `.d.ts` file.

It's a good start, but it doesn't include the mapping between parameter and result types. What we're really like to be able to write is a TypeScript function like this:

``` typescript
function sendRequest<T extends keyof RequestMethods>(
  key: T, params: MessageParams<T>
): Promise<ResponseResult<T>>;
```

If you had a function like this, plus the required [lookup types](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html), you could call it like this:

``` typescript
// The message params are typechecked
const result = await sendRequest("login", {username, password});

// The result is also typechecked!
console.log("Got profile picture: " + result.profilePicture);
```
