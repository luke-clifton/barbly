#! /usr/bin/env bash

jq -n '
    { title: "Example"
    , items:
        [ { label: "Say Hello", exec:["say", "Hello"]}
        , { }
        , { label: "Sub Menu"
          , items:
            [ {label: "DuckDuckGo", exec: ["open", "https://duckduckgo.com/"]}
            ]
          }
        , { label: "Information Only" }
        ]
    }'
