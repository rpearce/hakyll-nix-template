---
author: "My name"
authorTwitter: "@MyName"
desc: "I announce myself to the world"
image: "./images/waiheke-stony-batter.jpg"
keywords: "hello, announcement"
lang: "en"
title: "Hello, world!"
updated: "2020-09-22T12:00:00Z"
---

Hello, world! I am here!

<img
  alt="Grapevines among rolling hills leading to the sea"
  src="./images/waiheke-stony-batter.jpg"
  height="200"
/>

Haskell, for example:

```haskell
toSlug :: T.Text -> T.Text
toSlug =
  T.intercalate (T.singleton '-') . T.words . T.toLower . clean
```
