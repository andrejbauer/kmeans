module type COLLECTION =
sig
  type element
  type t
  val empty : t
  val add : element -> t -> t
  val singleton : element -> t
  val select : int -> t -> element list * t
  val fold : ('acc -> element -> 'acc) -> 'acc -> t -> 'acc
end

module type MEAN_SPACE =
sig
  type point
  module C : COLLECTION with type element = point

  val dist : point -> point -> point (** Distance *)
  val mean : C.t -> point (** Mean value of a collection *)
end

module type KMEANS =
sig
  type point
  module C : COLLECTION with type element = point
  val kmeans : int -> C.t -> C.t list * float
end

module Lloyd(S : MEAN_SPACE) : KMEANS with type point = S.point 
                                       and module C = S.C =
struct
  type point = S.point
  module C = S.C

  let kmeans k original_points =
    let centroids, points = C.select k points in
    let clusters =
      C.fold
        (fun clusters x -> C.add x )
        (List.map C.singleton centroids)
        points
      
    

end
