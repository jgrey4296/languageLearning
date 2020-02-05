# Heuristic Ordering using a Red Black Tree

    Specify some things, then sort them locally in a self-balancing tree.
    TODO: allow grouping into bins.

```
    (0): 03/28/17 11:55 jgrey:  python main.py 
    Starting 
    RBtree Heuristic Weights Test
    Specify all items now? y/[n]y
    Space Separated: hate anger loneliness fear devotion approval
    anger [<---->] hate
    :>
    loneliness [<---->] hate
    :>
    loneliness [<---->] anger
    :>
    fear [<---->] anger
    :>
    fear [<---->] loneliness
    :>
    devotion [<---->] anger
    :>
    devotion [<---->] loneliness
    :>
    devotion [<---->] fear
    :>
    approval [<---->] anger
    :>
    approval [<---->] fear
    :>
    approval [<---->] devotion
    :<
    6 Items Specified
    Final Order:
    
    hate < anger < loneliness < fear < approval < devotion
```
