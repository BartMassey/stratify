Require Export List.
Require Export stripPrefix.

Fixpoint stratify (X : Set) (eq: X -> X -> bool)
                  (s l: list X) {struct l}: list (list X) :=
  match s with
    | nil => l :: nil
    | _ => match l with
             | nil => nil :: nil
             | (e :: es) => match stripPrefix eq s l with
                              | Some l' => nil :: stratify X eq s l'
                              | None => match stratify X eq s es with
                                          | l0 :: ls => (e :: l0) :: ls
                                          | nil => nil
                                        end
                            end
           end
  end.
