# Lightning Scripts

Scripts for running a lightning node.
The LnTools library has tools for exploring the visible network given the connected peers.

You can also write math $2+2$ or 
$$
x = \int_0^t dN_t(s)
$$

## Choosing nodes to connect with
The program ChooseNodes expects a list of channels available. You might do something like 
```
$ lncli describegraph >> channels.json
```

If you want to use c-lightning with
```
$ lightningcli listchannels
```
then a few changes is needed in parsing the json file. See LnTools.hs.

The program also needs a list of nodes that you want to pay, or be able to reach, and a priority weight associated with each. This can be specified as a YAML file as such:
``` yaml
- node_id: $NODE_ID1
  weight: $WEIGHT1
- node_id: $NODE_ID2
  weight: $WEIGHT2
...
```



## Shortcomings
This approach ignores the issue of balances within channels. Channels are only measured by their capacities, which are public as opposed to balances. For a channel b/w A and B: Balance(A) + Balance(B) = Capacity(A<-->B).

Also, this is clearly a greedy algorithm. Possibility to choose 2 or more nodes simultaneously to minimize path but not clear how to deal with combinations like that.
