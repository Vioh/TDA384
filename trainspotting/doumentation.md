# How to run:

```
make
make run
```

All the codes that I modified are located in in ``Lab1.map`` and ``src/Lab1.java``.

# Classes:

These are the classes that are introduced:

* ``Train``: Represents the train program, which is just a java Thread.

* ``Map``: Represents the entire TSim map that was given.

* ``Direction``: Represents direction on the map, either towards station A (bottom), or station B (top).

* ``Position``: Represents the x-y position on the map, used for storing the positions of the sensors and the switches.

* ``Section``: Represents a *directed* rail section on the map that allows a train to travel in a certain direction. Each section has a single sensor placed at the end of the section, and shares the same resources (i.e. semaphore) with the section that supports travelling in the opposite direction.

* ``Track``: This is actually a subclass of ``Section``. But unlike a normal section, a track can be connected to other tracks via ``Connection``.

* ``Connection``: Represents the connection between a track and a next track, with a switch in between.

# TSim map design:

* For my solution, I use 20 different sensors, each sensor corresponds to either the start or the end of a ``Section``.

* This means there are also 20 different ``Section``. These sections are divided into 10 pairs, where each pair refers to the same locations on the map, but one is for heading towards A, and the other one is for heading towards B.

* Out of these 10 pairs, 8 of them are actually pairs of ``Tracks``. Each pair will have a semaphore for sharing the resources between the trains.

* The other 2 pairs of ``Section`` are for the intersection that lies close to station B. These 2 pairs will share the same semaphore, since these sections are part of a critical section.

* There are only 3 critical sections as shown in ``documentation.png``. These are the parts that are shared between both trains, but only one train is allowed to occupy at a time.

# Train behavior:

The main idea is to treat a ``Track`` and a **normal** ``Section`` separately, but with the same goal, which is to make sure that only 1 train has access to a critical section at a time.

For a ``Track``:
* If the sensor at the end of a track is active, we will stop the train, acquire the next track, and make the switch, before moving again.
* Once the sensor at the start a track is inactive, we simply release the previous track that the train was on, such that the other train can use it.

For a normal ``Section``:
* If the sensor at the start of the section is active, we will stop the train, acquire the section, before moving again.
* Once the sensor at the end of the section is inactive, we simply release the section so that the other train can use it.

The maximum train speed that works for my solution is 15. Due to the limitation of the train simulation, anything higher than than will cause a problem, since the simulation will not have enough time to report the ``SensorEvent`` back to the Train program, in order for us to make sensible decisions.

To test the solution, I ran the program 16 times, each for an hour and with a different combination train speeds, taken from the set {1, 5, 10, 15}.
