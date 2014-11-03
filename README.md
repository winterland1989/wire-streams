# cereal-io-streams - io-streams support for the cereal binary serialization library

## Functions
```haskell
putToStream :: Put -> IO (InputStream ByteString)
putEachStream :: Putter r -> InputStream r -> IO (InputStream ByteString)
getFromStream :: Get r -> InputStream ByteString -> IO r
getEachStream :: Get r -> InputStream ByteString -> IO (InputStream r)
contramapPut :: Putter r -> OutputStream ByteString -> IO (OutputStream r)
```

### Examples

```
>>> getFromStream (get :: Get String) =<< putToStream (put "serialize me")
"serialize me"
>>> getFromStream (get :: Get String) =<< Streams.fromByteString (Data.ByteString.drop 1 $ runPut $ put ("serialize me" :: String))
*** Exception: Get exception: too few bytes
From:	demandInput
```
