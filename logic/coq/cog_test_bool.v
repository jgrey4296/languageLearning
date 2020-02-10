Inductive bool : Type :=
| true
| false.

Definition negb (b:bool) : bool :=
  match b with
  | true => false
  | false => true
  end.

Definition andb (b1:bool) (b2:bool) : bool :=
  match b1 with
  | true => b2
  | false => false
  end.

Example test_negb: (negb false) = true.
Proof. simpl. reflexivity. Qed.

Example test_negb2: (negb true) = false.
Proof. simpl. reflexivity. Qed.

Compute (andb true (negb false)).
