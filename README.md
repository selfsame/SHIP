SHIP
====

A spaceship horror simulation in clojure.

The goal of this project is to stay away from mutable state as much as possible.

Currently it uses a history record of all events, which can be lazily mapped to find out things like where an entity is, who knows who, etc.

I'm also experimenting with core.logic in hope that it can help the actors create complex behavior based on fulfulling an abstract goal.






