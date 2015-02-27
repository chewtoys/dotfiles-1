{:user {:plugins [[lein-kibit "0.0.8"]
                  [lein-bikeshed "0.2.0"]
                  [lein-cljfmt "0.1.4"]
                  [lein-ancient "0.6.3"]
                  [jonase/eastwood "0.2.1"]
                  [cider/cider-nrepl "0.8.2"]
                  [venantius/ultra "0.2.0"]]
        :dependencies [[cljfmt "0.1.7"]
                       [jonase/eastwood "0.2.1" :exclusions  [org.clojure/clojure]]]
        :ultra {:color-scheme :solarized_dark}
        :eastwood {:exclude-linters [:constant-test :suspicious-expression :local-shadows-var]}
        }}
