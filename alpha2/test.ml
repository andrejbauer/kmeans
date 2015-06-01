type distance =
    | Num of num_dist
    | Set of set_dist
    | Tup of tuple_dist
    | Dis of discrete_distance
    | Seq of sequence_dist
and num_dist =
    | Rampa of float*float
    | NDelta
and discrete_distance =
    | DDelta
and tuple_dist =
    | Max of distance list
    | Euc of distance list
    | Avg of distance list
    | Med of distance list
    | Har of distance list
    | Geo of distance list
    | Wgh of float list*distance
and set_dist =
    | SetSample of distance
and sequence_dist =
    | SeqSample of distance