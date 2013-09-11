# complej

A clojure library designed to study Complex Networks.

## Usage

	(use 'complej.core)
	(def g (barabasi 1024 4))
	(histogram (degrees g))
	(plot g)

## License

Copyright © 2013 Bruno Kim Medeiros Cesar

Distributed under the Eclipse Public License, the same as Clojure.
