type data =
    | Discrete of string
    | Numerical of float
    | Set of data list
    | Tuple of data list
    | Sequence of data list
    | Missing

type schema =
    | N of float*float
    | D of string list
    | Tp of schema list
    | St of schema
    | Sq of schema