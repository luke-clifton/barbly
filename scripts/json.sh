#! /usr/bin/env bash

jq -n '
    { title: "Hello"
    , items:
        [ { label: "Welp", exec:["say","welp"]}
        , { label: "sdA", items:[]}
        , { label: "Sub Menu"
          , items:
            [ {label: "Ok", exec: ["open", "https://www.google.com"]}
            , {label: "You?"}
            ]
          }
        ]
    }'
