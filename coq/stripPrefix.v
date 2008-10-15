Require Export List.

Fixpoint stripPrefix (X: Set) (eq: X -> X -> bool)
                     (s l : list X) {struct s}: option (list X) :=
  match s with
    | nil => Some l
    | c :: cs => match l with
                   | nil => None
                   | e :: es => if eq c e
                                then stripPrefix X eq cs es
                                else None
                 end
  end.


Implicit Arguments stripPrefix [X].

