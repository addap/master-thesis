From iris.algebra Require Import excl_auth gset gmap agree csum frac.
From iris.base_logic.lib Require Import invariants iprop wsat saved_prop.
From iris.program_logic Require Export weakestpre.
From iris.heap_lang Require Import notation proofmode.
From iris.prelude Require Import options.

(** Stack 1: No helping, bag spec. *)

Definition change_state : val := λ: <>, #().
Definition new_stack : val := λ: "_", ref NONEV.
Definition push : val :=
  rec: "push" "s" "v" :=
    let: "tail" := ! "s" in
    let: "new" := SOME (ref ("v", "tail")) in
    if: CAS "s" "tail" "new" then #() else "push" "s" "v".
Definition pop : val :=
  rec: "pop" "s" :=
    match: !"s" with
      NONE => NONEV
    | SOME "l" =>
      let: "pair" := !"l" in
      if: CAS "s" (SOME "l") (Snd "pair")
      then SOME (Fst "pair")
      else "pop" "s"
    end.

Class deferredGpreS Σ := {
  assums_mapG :> inG Σ (authR (gmapUR (gname * gname) unitR));
  (* a.d. could maybe be merged with the other token *)
  exclusive :> inG Σ (exclR unitR);
  leftrightG :> inG Σ (prodUR 
                        (optionUR (exclR unitR))
                        (optionUR (exclR unitR))
                      );
  torchG :> inG Σ (csumR fracR (agreeR unitO));
}.

Definition deferredΣ := #[ 
  GFunctor (authR (gmapUR (gname * gname) unitR));
  GFunctor (exclR unitR);
  GFunctor (prodUR 
              (optionUR (exclR unitR))
              (optionUR (exclR unitR))
            );
  GFunctor (csumR fracR (agreeR unitO))
].

Global Instance subG_deferredΣ {Σ} : subG deferredΣ Σ → deferredGpreS Σ.
Proof. solve_inG. Qed.

Class deferredGS Σ := {
  deferred_inG :> deferredGpreS Σ; 
  map_name : gname;
  mode_name : gname;
  deferred_prop_name : gname;
}.

Section stacks.
  Context `{!heapGS Σ, !deferredGS Σ, !savedPropG Σ} (N : namespace).
  Implicit Types l : loc.

  Definition oloc_to_val (ol: option loc) : val :=
    match ol with
    | None => NONEV
    | Some loc => SOMEV (#loc)
    end.
  Local Instance oloc_to_val_inj : Inj (=) (=) oloc_to_val.
  Proof. intros [|][|]; simpl; congruence. Qed.

  Definition is_list_pre (P : valO -d> iPropO Σ) (F : option loc -d> iPropO Σ) :
     option loc -d> iPropO Σ := λ v, match v with
     | None => True
     | Some l => ∃ (h : val) (t : option loc), l ↦□ (h, oloc_to_val t)%V ∗ P h ∗ ▷ F t
     end%I.

  Local Instance is_list_contr (P : valO -d> iPropO Σ) : Contractive (is_list_pre P).
  Proof. solve_contractive. Qed.

  Definition is_list_def (P : valO -d> iPropO Σ) := fixpoint (is_list_pre P).
  Definition is_list_aux P : seal (@is_list_def P). Proof. by eexists. Qed.
  Definition is_list P := unseal (is_list_aux P).
  Definition is_list_eq P : @is_list P = @is_list_def P := seal_eq (is_list_aux P).

  Lemma is_list_unfold (P : valO -d> iPropO Σ) v :
    is_list P v ⊣⊢ is_list_pre P (is_list P) v.
  Proof.
    rewrite is_list_eq. apply (fixpoint_unfold (is_list_pre P)).
  Qed.

  Lemma is_list_unfold2 (P : valO -d> iPropO Σ) v :
    is_list P v ≡ is_list_pre P (is_list P) v.
  Proof.
    rewrite is_list_eq. apply (fixpoint_unfold (is_list_pre P)).
  Qed.

  Lemma is_list_dup (P : val → iProp Σ) v :
    is_list P v -∗ is_list P v ∗ match v with
      | None => True
      | Some l => ∃ h t, l ↦□ (h, oloc_to_val t)%V
      end.
  Proof.
    iIntros "Hstack". iDestruct (is_list_unfold with "Hstack") as "Hstack".
    destruct v as [l|].
    - iDestruct "Hstack" as (h t) "(#Hl & ? & ?)".
      rewrite (is_list_unfold _ (Some _)). iSplitL; iExists _, _; by iFrame.
    - rewrite is_list_unfold; iSplitR; eauto.
  Qed.

  Lemma is_list_defer_Q (Q : iProp Σ) (P : val → iProp Σ) v :
    ⊢ is_list (λ x, ▷ (P x)) v -∗ is_list (λ x, ▷ (Q -∗ P x)) v.
  Proof.
    iIntros "Hstack".
    iLöb as "IH" forall (v).
    rewrite is_list_unfold2 /is_list_pre.
    rewrite is_list_unfold2 /is_list_pre.
    destruct v as [l|].
    - iDestruct "Hstack" as (h t) "(#Hl & HP & Hres)".
      iExists _, _. iFrame "Hl".
      iSplitR "Hres".
      + iNext. iIntros "_". iApply "HP".
      + iNext. by iApply "IH".
    - done.
  Qed.

  Lemma is_list_resolve_Q (Q : iProp Σ) (P : val → iProp Σ) v :
    ⊢ ▷ □ Q -∗ is_list (λ x, ▷ (Q -∗ P x)) v -∗ is_list (λ x, ▷ (P x)) v.
  Proof.
    iIntros "#HQ Hstack".
    iLöb as "IH" forall (v).
    rewrite is_list_unfold2 /is_list_pre.
    rewrite is_list_unfold2 /is_list_pre.
    destruct v as [l|].
    - iDestruct "Hstack" as (h t) "(#Hl & HP & Hres)".
      iExists _, _. iFrame "Hl".
      iSplitR "Hres".
      + iNext. by iApply "HP". 
      + iNext. by iApply "IH".
    - done.
  Qed.

  (* We have a set of gnames, for each gname in the set we keep track of the following state machine.
   *)

  Definition element_token δ := own δ (Excl tt).
  Definition full_token γ := own γ (Excl' tt, Excl' tt).
  Definition left_token γ := own γ (Excl' tt, None).
  Definition right_token γ := own γ (None, Excl' tt).
  Notation normal_mode_token := left_token (only parsing).
  Notation deferred_mode_token := right_token (only parsing).

  (* Definition full_oneshot γ := own γ (Cinl 1%Qp).
  Definition half_oneshot γ := own γ (Cinl (1/2)%Qp).
  Definition agree_aftershot γ := own γ (Cinr (to_agree tt)).
  Notation request_full := full_oneshot (only parsing).
  Notation request_waiting := half_oneshot (only parsing).
  Notation request_fulfilled := agree_aftershot (only parsing). *)

  Definition is_element γs := own map_name (◯ {[γs := tt]}).
  Definition is_element_map (M : gmap (gname * gname) unit) := own map_name (● M).

  Definition element_state γ δ := (
    ∃ Q, saved_prop_own γ DfracDiscarded Q ∗ ▷ □ Q ∗ element_token δ
  )%I.
  
  Definition deferred_Q Q := saved_prop_own deferred_prop_name (DfracOwn (1/2)) Q.
  Definition deferred_element_state Q γ δ := (
    deferred_Q Q ∗ 
    saved_prop_own γ DfracDiscarded Q ∗
    element_token δ
  )%I.

  Definition normal_mode_map := (
    ∃ M, is_element_map M ∗
      [∗ map] γs ↦ tt ∈ M, let '(γ, δ) := γs in
        element_state γ δ
  )%I.

  Definition deferred_mode_map Q := (
    ∃ (γd δd : gname) M, is_element_map (<[(γd, δd) := tt]> M) ∗
      (⌜M !! (γd, δd) = None⌝ ∗ 
        deferred_element_state Q γd δd) ∗
      [∗ map] γs ↦ tt ∈ M, let '(γ, δ) := γs in
        element_state γ δ
  )%I.

  Definition normal_mode P ol' := (
    is_list (λ x, ▷ P x) ol' ∗
    normal_mode_token mode_name ∗ 
    saved_prop_own deferred_prop_name (DfracOwn 1) True ∗ 
    normal_mode_map
  )%I.

  Definition deferred_mode (Q : iPropO Σ) P ol' := (
    is_list (λ x, ▷ (Q -∗ P x)) ol' ∗
    deferred_mode_token mode_name ∗
    deferred_mode_map Q
  )%I.

  Lemma mode_is_list_dup (P : val → iProp Σ) v :
    (normal_mode P v ∨ ∃ Q, deferred_mode Q P v) -∗ 
      (normal_mode P v ∨ ∃ Q, deferred_mode Q P v) ∗ 
      match v with
      | None => True
      | Some l => ∃ h t, l ↦□ (h, oloc_to_val t)%V
      end.
  Proof.
    iIntros "[(Hlist & HRest)|(% & Hlist & HRest)]".
    - iPoseProof (is_list_dup with "Hlist") as "[Hlist H]".
      iFrame. iLeft. iFrame.
    - iPoseProof (is_list_dup with "Hlist") as "[Hlist H]". 
      iFrame. iRight. iExists _. iFrame.
  Qed.

  Definition stack_inv P v := (
    ∃ l ol', ⌜v = #l⌝ ∗ l ↦ oloc_to_val ol' ∗ (
      normal_mode P ol' ∨ ∃ (Q : iProp Σ), deferred_mode Q P ol' 
  ))%I.

  Definition is_stack (P : valO -d> iPropO Σ) v :=
    inv N (stack_inv P v).
  Definition is_stack_reader (P : valO -d> iProp Σ) v :=
    (is_stack P v ∗ deferred_mode_token mode_name)%I.
  Definition fulfill_permission Q := (
    normal_mode_token mode_name ∗ deferred_Q Q
  )%I.
  Definition push_permission γ δ (Q : iProp Σ) := (
    saved_prop_own γ DfracDiscarded Q ∗
    is_element (γ, δ)
  )%I.
  
  Section element_token.
    Lemma element_token_create : 
      ⊢ |==> ∃ δ, element_token δ.
    Proof.
      iMod (own_alloc (Excl tt)) as (δ) "Het"; first done. 
      iExists δ. by iFrame.
    Qed.

    Lemma element_token_exclusive δ :
      element_token δ -∗ element_token δ -∗ False.
    Proof.
      iIntros "H1 H2".
      iDestruct (own_valid_2 with "H1 H2") as "%H".
      exfalso.
      by compute.
    Qed.
  End element_token.
  

  Section instances.

  Global Instance is_list_dist_later ol n :
    Proper ((dist_later n) ==> (dist n)) (λ (P : valO -d> iPropO Σ), is_list (λ x, ▷ P x)%I ol).
  Proof.
    induction (lt_wf n) as [n _ IH] in ol =>P P' HPeq.
    destruct ol as [l|]; rewrite !is_list_unfold2 /is_list_pre.
    - f_equiv. intros h.
      f_equiv. intros t.
      f_equiv. f_equiv.
      + f_contractive.
        rewrite /dist_later in HPeq.
        f_equiv.
      + f_contractive.
        apply IH; first lia.
        rewrite /dist_later.
        rewrite /dist_later in HPeq.
        destruct n as [|n]; first done.
        by apply dist_S.
    - done.
  Qed.

  Global Instance is_list_dist_later_Q ol n Q :
    Proper ((dist_later n) ==> (dist n)) (λ (P : valO -d> iPropO Σ), is_list (λ x, ▷ (Q -∗ P x))%I ol).
  Proof.
    induction (lt_wf n) as [n _ IH] in ol =>P P' HPeq.
    destruct ol as [l|]; rewrite !is_list_unfold2 /is_list_pre.
    - f_equiv. intros h.
      f_equiv. intros t.
      f_equiv. f_equiv.
      + f_contractive.
        rewrite /dist_later in HPeq.
        f_equiv.
        f_equiv.
      + f_contractive.
        apply IH; first lia.
        rewrite /dist_later.
        rewrite /dist_later in HPeq.
        destruct n as [|n]; first done.
        by apply dist_S.
    - done.
  Qed.

  Global Instance is_stack_ne q n :
    Proper ((dist n) ==> (dist n)) (λ P, is_stack P q).
  Proof.
    intros P P' HPeq.
    rewrite /is_stack.
    (* f_contractive. *)
    f_equiv.
    rewrite /stack_inv.
    f_equiv. intros l.
    f_equiv. intros ol.
    do 2 f_equiv.
    f_equiv.
    - rewrite /normal_mode.
      f_equiv.
      apply is_list_dist_later.
      rewrite /dist_later.
      destruct n as [|n]; first done.
      by apply dist_S.
    - rewrite /deferred_mode.
      f_equiv. intros Q.
      f_equiv.
      apply is_list_dist_later_Q.
      rewrite /dist_later.
      destruct n as [|n]; first done.
      by apply dist_S.
  Qed.

  Global Instance is_stack_reader_ne q n :
    Proper ((dist n) ==> (dist n)) (λ P, is_stack_reader P q).
  Proof.
    intros P P' HPeq.
    rewrite /is_stack_reader.
    f_equiv.
    by apply is_stack_ne.
  Qed.

  Global Instance is_stack_Persistent q P :
    Persistent (is_stack P q).
  Proof. by apply _. Qed.

  Global Instance push_permission_Persistent γ δ Q : Persistent (push_permission γ δ Q).
  Proof. by apply _. Qed.

  End instances.


  Section leftright_tokens.
    Lemma left_token_exclusive γ : 
      ⊢ left_token γ -∗ left_token γ -∗ False.
    Proof.
      iIntros "H1 H2".
      iDestruct (own_valid_2 with "H1 H2") as "%H".
      exfalso.
      move: H.
      rewrite -pair_op pair_valid.
      (* a.d. TODO how does ssreflect work again? *)
      case=> H _; move: H.
      by compute.
    Qed.

    Lemma right_token_exclusive γ : 
      ⊢ right_token γ -∗ right_token γ -∗ False.
    Proof.
      iIntros "H1 H2".
      iDestruct (own_valid_2 with "H1 H2") as "%H".
      exfalso.
      move: H.
      rewrite -pair_op pair_valid.
      case=> _.
      by compute.
    Qed.

  End leftright_tokens.

  (* Section request_states.
    Global Instance request_fulfilled_Persistent γ : Persistent (request_fulfilled γ).
    Proof. by apply _. Qed.

    Lemma request_full_create : ⊢ |==> ∃ γ, request_full γ.
    Proof. by iMod (own_alloc (Cinl 1%Qp)) as (γ) "Hps"; last iExists γ. Qed.

    Lemma request_split γ :
      ⊢ request_full γ ==∗ request_waiting γ ∗ request_waiting γ.
    Proof.
      rewrite /request_full /request_waiting.
      rewrite -own_op.
      iApply own_update.
      rewrite -Cinl_op.
      apply csum_update_l.
      rewrite frac_op.
      rewrite cmra_update_updateP.
      apply cmra_updateP_id. 
      by apply Qp.half_half.
    Qed. 

    Lemma request_join γ :
      ⊢ request_waiting γ ∗ request_waiting γ ==∗ request_full γ.
    Proof.
      rewrite /request_full /request_waiting.
      rewrite -own_op.
      iApply own_update.
      rewrite -Cinl_op.
      apply csum_update_l.
      rewrite frac_op.
      rewrite cmra_update_updateP.
      apply cmra_updateP_id. 
      by rewrite Qp.half_half.
    Qed.

    Lemma request_fulfill γ :
      ⊢ request_full γ ==∗ request_fulfilled γ.
    Proof.
      iApply own_update.
      apply cmra_update_exclusive.
      apply Cinr_valid.
      done.
    Qed.
    
    Lemma request_disjoint γ : (request_waiting γ ∗ request_fulfilled γ) -∗ False.
    Proof. 
      by rewrite  -own_op own_valid csum_validI.
    Qed.

  End request_states. *)

  Section element_map.
    (* a.d. TODO using the fulfiller map and the map in the respective mode it should be possible to go 
    from one mode to the other. *)
    Global Instance is_element_Persistent γs : Persistent (is_element γs). 
    Proof. by apply _. Qed.

    Lemma update_element_map M γs :
      M !! γs = None → 
        is_element_map M ==∗
          is_element_map (<[(γs) := tt]> M) ∗ is_element γs.
    Proof.
      intros Hlkp. iIntros "HM".
      iMod (own_update with "HM") as "[HM HiP]".
      { apply (@auth_update_alloc (gmapUR _ _) M).
        apply (alloc_singleton_local_update _ (γs) tt);
        [apply Hlkp|done]. }
      by iFrame. 
    Qed.

    Lemma claim_membership M γs :
      is_element_map M ∗ is_element γs -∗
        ⌜ M !! γs = Some tt ⌝.
    Proof.
      rewrite /is_element_map /is_element.
      rewrite -own_op own_valid auth_both_validI /=.
      iIntros "(HM & #HpM)". iDestruct "HM" as (M') "#HM".
      rewrite gmap_equivI gmap_validI.
      iSpecialize ("HM" $! (γs)). iSpecialize ("HpM" $! (γs)).
      rewrite lookup_op lookup_singleton.
      rewrite option_equivI.
      case: (M  !! (γs))=> [[]|] /=; [|
      case: (M' !! (γs))=> [[]|] /=; by iExFalso].
      done.
    Qed.

    Lemma deferred_element_state_non_duplicable Q γ γ' δ :
      deferred_element_state Q γ δ -∗ element_state γ' δ -∗ False.
    Proof.
      rewrite /deferred_element_state /element_state.
      iIntros "(_ & _ & Het) (% & _ & _ & Het')".
      by iDestruct (element_token_exclusive with "Het Het'") as "%HFalse".
    Qed.

    Lemma element_state_non_duplicable γ γ' δ :
      element_state γ δ -∗ element_state γ' δ -∗ False.
    Proof.
      rewrite /element_state.
      iIntros "(% & _ & _ & Het) (% & _ & _ & Het')".
      by iDestruct (element_token_exclusive with "Het Het'") as "%HFalse".
    Qed.

    Lemma deferred_element_create Q :
      ⊢ saved_prop_own deferred_prop_name (DfracOwn 1) True ==∗
          deferred_Q Q ∗ ∃ γ δ, deferred_element_state Q γ δ.
    Proof.
      iIntros "HglobQ".
      iMod (element_token_create) as (δ) "Het".
      iMod (saved_prop_alloc Q DfracDiscarded) as (γ) "HQ"; first done.
      iMod (saved_prop_update Q with "HglobQ") as "HglobQ".
      iDestruct (fractional.fractional_half_1 with "HglobQ") as "[HglobQ HglobQ']".
      iFrame "HglobQ'".
      iExists γ, δ.
      by iFrame.
    Qed.

    Lemma update_normal_mode_map Q γ δ :
      ⊢ normal_mode_map -∗ 
        deferred_element_state Q γ δ ==∗
          deferred_mode_map Q ∗ is_element (γ, δ).
    Proof.
      iIntros "(% & HM & Helms) Hdelm".
      destruct (M !! (γ, δ)) as [Ψ|] eqn:Hlkp.
      - rewrite (big_opM_delete _ _ _ _ Hlkp).
        iDestruct "Helms" as "(Helm & Helms)".
        by iDestruct (deferred_element_state_non_duplicable with "Hdelm Helm") as "HFalse".
      - rewrite /deferred_mode_map.
        iMod (update_element_map M (γ, δ) Hlkp with "HM") as "[HM Hmem]".
        iFrame "Hmem".
        iModIntro. iExists γ, δ, M.
        by iFrame.
    Qed.

    Lemma normal_mode_to_deferred_mode P ol' Q :
      ⊢ normal_mode P ol' -∗ 
        deferred_mode_token mode_name ==∗
          (deferred_mode Q P ol' ∗ normal_mode_token mode_name ∗ 
           deferred_Q Q ∗
           ∃ γ δ, push_permission γ δ Q).
    Proof.
      iIntros "(Hlist & Hmtok' & HglobQ & HM) Hmtok".
      iMod (deferred_element_create Q with "HglobQ") as "[HglobQ' (% & % & Hdelm)]".
      rewrite /deferred_element_state.
      iDestruct "Hdelm" as "(HglobQ & #HQ & Het)".
      iFrame "Hmtok'".
      rewrite /deferred_mode.
      iMod (update_normal_mode_map Q γ δ with "HM [$]") as "[HM Hmem]".
      iFrame.
      iModIntro.
      iSplitL "Hlist".
      - iApply (is_list_defer_Q with "Hlist").
      - iExists _, _. by iFrame.
    Qed.

    Lemma deferred_element_resolve γ δ Q Q' :
      ⊢ deferred_element_state Q γ δ -∗ 
        ▷ □ Q -∗ 
        deferred_Q Q' ==∗ 
          element_state γ δ ∗ 
          saved_prop_own deferred_prop_name (DfracOwn 1) True.
    Proof.
      iIntros "(HglobQ & HQ & Het) Hq HglobQ'".
      iAssert (|==> saved_prop_own deferred_prop_name (DfracOwn 1) Q)%I with "[HglobQ HglobQ']" as "HglobQ".
      { iMod (saved_prop_update_2 Q with "HglobQ HglobQ'") as "[HglobQ HglobQ']".
        1: apply Qp.half_half.
        iModIntro.
        by iApply (fractional.fractional_half_2 with "HglobQ HglobQ'"). }
      iMod "HglobQ".
      rewrite /element_state.
      iSplitR "HglobQ".
      - iExists Q. by iFrame.
      - iApply (saved_prop_update with "HglobQ").
    Qed.

    Lemma deferred_mode_to_normal_mode P ol' Q Q' :
      ⊢ deferred_mode Q P ol' -∗ 
        ▷ □ Q -∗
        normal_mode_token mode_name -∗ 
        deferred_Q Q' ==∗
          normal_mode P ol' ∗ deferred_mode_token mode_name.
    Proof.
      iIntros "(Hlist & Hmtok' & (% & % & % & HM & (%Hnone & Hdelm) & Helms)) #Hq Hmtok HglobQ".
      iMod (deferred_element_resolve with "Hdelm Hq HglobQ") as "[Helm HglobQ]".
      iFrame "Hmtok'".
      rewrite /normal_mode.
      iFrame "HglobQ Hmtok".
      iSplitL "Hlist".
      - iApply (is_list_resolve_Q with "Hq Hlist").
      - rewrite /normal_mode_map.
        iExists (<[(γd, δd):=tt]> M).
        iFrame "HM".
        iModIntro.
        iApply (big_opM_insert); first done.
        iFrame.
    Qed.

    Lemma lookup_element γ δ :
      normal_mode_map -∗ is_element (γ, δ) -∗
        ((element_state γ δ -∗ normal_mode_map) ∗ element_state γ δ).
    Proof.
      iIntros "Helms Hmem". rewrite /normal_mode_map.
      iDestruct "Helms" as (M) "[HM Helms]".
      iDestruct (claim_membership M (γ, δ) with "[$]") as "%Hlkp".
      iDestruct (big_sepM_delete _ _ (γ, δ) with "Helms")
        as "[Hes Helms]"; first done.
      iSplitL "Helms HM".
      - iIntros "Hes". iExists M. iFrame.
        rewrite (big_opM_delete _ _ _ _ Hlkp). iFrame.
      - done.
    Qed.

    Lemma lookup_deferred_element γ δ Q :
      deferred_mode_map Q -∗ is_element (γ, δ) -∗ (
          ((element_state γ δ -∗ deferred_mode_map Q) ∗ element_state γ δ)
        ∨ ((deferred_element_state Q γ δ -∗ deferred_mode_map Q) ∗ deferred_element_state Q γ δ)
      ).
    Proof.
      iIntros "Helms Hmem". rewrite /deferred_mode_map.
      iDestruct "Helms" as (γd δd M) "(HM & (%Hin & Hstate) & Helms)".
      iDestruct (claim_membership (<[(γd, δd):=tt]> M) (γ, δ) with "[$]") as "%Hlkp".
      (* CA over whether (γ, δ) = (γd, δd).
         If they are equal, we return a deferred element 
         Otherwise, an element already exists and we return that. *)
      destruct (decide ((γ, δ) = (γd, δd))) as [Heq|Hne].
      - iRight. 
        inversion Heq. simplify_eq.
        iFrame "Hstate".
        iIntros "Hstate".
        iExists _, _, _. by iFrame.
      - iLeft.
        rewrite lookup_insert_ne in Hlkp; last done.
        iDestruct (big_sepM_delete _ _ (γ, δ) with "Helms")
          as "[Hes Helms]"; first done.
        iSplitL "Helms HM Hstate".
        + iIntros "Hes". iExists _, _, M. iFrame.
          rewrite (big_opM_delete _ _ _ _ Hlkp). by iFrame.
        + done.
    Qed.

    (* Lemma lookup_deferred_element γs :
      deferred_elements -∗ is_element γs -∗
        ((element_state γs -∗ deferred_elements) ∗ element_state γs).
    Proof.
      iIntros "Helms Hmem". rewrite /deferred_elements.
      iDestruct "Helms" as (M) "[HM Helms]".
      iDestruct (claim_membership M γs with "[$]") as "%Hlkp".
      iDestruct (big_sepM_delete _ _ (γs) with "Helms")
        as "[Hes Helms]"; first done.
      iSplitL "Helms HM".
      - iIntros "Hes". iExists M. iFrame.
        rewrite (big_opM_delete _ _ _ _ Hlkp). iFrame.
      - done.
    Qed. *)
    
    (* Lemma lookup_promiseInv_inner p γ ε Ε :
      ▷ promiseInv_inner -∗ isMember p γ ε ={Ε}=∗
        (▷ (promiseSt p γ ε -∗ promiseInv_inner) ∗ promiseSt_later p γ ε).
    Proof.
      iIntros "HpInv #Hmem". 
      iAssert (▷ ((promiseSt p γ ε -∗ promiseInv_inner) ∗ promiseSt p γ ε))%I with "[HpInv]" as "(HpClose & HpSt)".
      2: iFrame; by iApply later_promiseSt_promiseSt_later.
      iModIntro. 
      iApply (lookup_promiseInv_inner' with "HpInv Hmem").
    Qed. *)
  End element_map.

End stacks.

Section proofs.
  Context `{!heapGS Σ} `{!deferredGS Σ} `{!savedPropG Σ} (N : namespace).

  Notation normal_mode_token := left_token.
  Notation deferred_mode_token := right_token.

  (* Lemma full_token_create : 
    ⊢ |==> ∃ γ, full_token γ.
  Proof.
    iMod (own_alloc (Excl' tt, Excl' tt)) as (γ) "Hes"; first done. 
    iExists γ. by iFrame.
  Qed. *)

  Theorem register_push_Q (Q : iProp Σ) P s :
    {{{ is_stack_reader N P s }}}
      change_state #()
    {{{ RET #(); 
        fulfill_permission Q ∗
        ∃ γ δ, push_permission γ δ Q }}}.
  Proof.
    iIntros (Φ) "(Hstack & Hmtok) HΦ". 
    iInv N as (ℓ v') "(>% & Hl & [Hmode|Hmode])" "Hclose"; subst.
    2: {
      iDestruct "Hmode" as (Q') "(_ & Hmtok' & _)".
      wp_lam.
      iDestruct (right_token_exclusive with "Hmtok Hmtok'") as "[]".
    }
    wp_lam.
    iMod (normal_mode_to_deferred_mode _ _ Q with "Hmode Hmtok") as "(Hmode & Hmtok & HglobQ & Hpush)".
    iMod ("Hclose" with "[Hmode Hl]") as "_".
    { iNext. iExists _, _. iFrame. 
      iSplit; first done.
      iRight. by iExists _. }
    iApply "HΦ".
    by iFrame "Hpush Hmtok HglobQ".
  Qed.

  Theorem push_spec γ δ (Q : iProp Σ) P s v :
    {{{ is_stack N P s ∗ push_permission γ δ Q ∗ ▷ (Q -∗ P v) }}} 
      push s v 
    {{{ RET #(); True }}}.
  Proof.
    iIntros (Φ) "(#Hstack & (#HQ & #Hin) & HQP) HΦ".
    iLöb as "IH".
    wp_lam. wp_let. wp_bind (Load _).
    iInv N as (ℓ v') "(>% & Hl & Hlist)" "Hclose"; subst.
    wp_load.
    iMod ("Hclose" with "[Hl Hlist]") as "_".
    { iNext; iExists _, _; by iFrame. }
    iModIntro. wp_let. wp_alloc ℓ' as "Hl'". wp_pures. wp_bind (CmpXchg _ _ _).
    iInv N as (ℓ'' v'') "(>% & >Hl & Hlist)" "Hclose"; simplify_eq.
    destruct (decide (v' = v'')) as [->|Hne].
    - wp_cmpxchg_suc. { destruct v''; left; done. }
      iMod (mapsto_persist with "Hl'") as "Hl'".
      iMod ("Hclose" with "[HQP Hl Hl' Hlist]") as "_".
      { iNext; iExists _, (Some ℓ'); iFrame. iSplit; first done.
        (* now we can check in which mode we are anyways. *)  
        iDestruct "Hlist" as "[(Hlist & Hmtok & HglobQ & HM)|(%Q'' & Hmode)]".
        - iLeft.
          rewrite /normal_mode.
          iFrame "Hmtok HglobQ".
          rewrite (is_list_unfold2 _ (Some _)) /=.
          iPoseProof (lookup_element with "HM Hin") as "[Hclose (%Q' & #HQ' & #Hq & Het)]".
          iPoseProof (saved_prop_agree with "HQ HQ'") as "Heq".
          iSplitR "Hclose Het".
          + iExists _, _. iFrame.
            iNext. iApply "HQP". by iRewrite "Heq".
          + iApply "Hclose". iExists Q'. iFrame. by iSplit.
        - iRight.
          rewrite /deferred_mode.
          iDestruct "Hmode" as "(Hlist & Hmtok & HM)".
          iExists Q''.
          iFrame "Hmtok".
          rewrite (is_list_unfold2 _ (Some _)) /=.
          iPoseProof (lookup_deferred_element with "HM Hin") as "[[Hclose (%Q' & #HQ' & #Hq & Het)]
                                                                 |[Hclose (HglobQ & #HQ' & Het)]]".
          { iPoseProof (saved_prop_agree with "HQ HQ'") as "Heq".
            iSplitR "Hclose Het".
            + iExists _, _. iFrame.
              iNext. iIntros "_". iApply "HQP". by iRewrite "Heq".
            + iApply "Hclose". iExists Q'. iFrame. by iSplit. }
          { iPoseProof (saved_prop_agree with "HQ HQ'") as "Heq".
            iSplitR "Hclose Het HglobQ".
            + iExists _, _. iFrame.
              iNext. iIntros "Hq''". iApply "HQP". by iRewrite "Heq".
            + iApply "Hclose". iFrame. by done. }
      }
      iModIntro.
      wp_pures.
      by iApply "HΦ".
    - wp_cmpxchg_fail.
      { destruct v', v''; simpl; congruence. }
      { destruct v''; left; done. }
      iMod ("Hclose" with "[Hl Hlist]") as "_".
      { iNext; iExists _, _; by iFrame. }
      iModIntro.
      wp_pures.
      iApply ("IH" with "HQP HΦ").
  Qed.

  Theorem fulfill_Q (Q : iProp Σ) P s :
    {{{ is_stack N P s ∗
        fulfill_permission Q ∗
        □ Q }}}
      change_state #() 
    {{{ RET #(); 
        is_stack_reader N P s }}}.
  Proof.
    iIntros (Φ) "(#Hstack & (Hmtok & Hfulfill) & #Hq) HΦ".
    iInv N as (ℓ v') "(>% & Hl & [Hmode|(%Q' & Hmode)])" "Hclose"; subst.
    {
      iDestruct "Hmode" as "(_ & Hmtok' & _)".
      wp_lam.
      iDestruct (left_token_exclusive with "Hmtok Hmtok'") as "[]".
    }
    wp_lam.
    iDestruct "Hmode" as "(Hlist & Hmtok' & % & % & % & HM & (% & HglobQ' & Helm) & Helms)".
    iAssert (▷ □ Q')%I with "[Hfulfill HglobQ']" as "#Hq'".
    { iPoseProof (saved_prop_agree with "Hfulfill HglobQ'") as "Heq".
      iNext. by iRewrite -"Heq". }
    iMod (deferred_mode_to_normal_mode _ _ Q' with "[Hlist Hmtok' HM HglobQ' Helm Helms] Hq' Hmtok Hfulfill") 
      as "(Hmode & Hmtok)".
    { iFrame. iExists _, _, _. by iFrame. }
    iMod ("Hclose" with "[Hmode Hl]") as "_".
    { iNext. iExists _, _. 
      iSplit; first done.
      by iFrame. }
    iModIntro.
    iApply "HΦ".
    by iFrame.
  Qed.

  Theorem pop_spec P s :
    {{{ is_stack_reader N P s }}} 
      pop s 
    {{{ ov, RET ov; is_stack_reader N P s ∗ (⌜ov = NONEV⌝ ∨ ∃ v, ⌜ov = SOMEV v⌝ ∗ P v) }}}.
  Proof.
    iIntros (Φ) "(#Hstack & Hmtok) HΦ".
    iLöb as "IH".
    wp_lam. wp_bind (Load _).
    iInv N as (ℓ v') "(>% & Hl & Hlist)" "Hclose"; subst.
    iDestruct (mode_is_list_dup with "Hlist") as "[Hlist Hlist2]".
    wp_load.
    iMod ("Hclose" with "[Hl Hlist]") as "_".
    { iNext; iExists _, _; by iFrame. }
    iModIntro.
    destruct v' as [l|]; last first.
    - wp_match.
      iApply "HΦ". 
      iModIntro. iFrame. iSplit; first done.
      by iLeft.
    - wp_match. wp_bind (Load _).
      iInv N as (ℓ' v') "(>% & Hl' & [Hlist|Hlist])" "Hclose"; simplify_eq.
      2: {
        iDestruct "Hlist" as "(% & _ & >Hmtok' & _)".
        iDestruct (right_token_exclusive with "Hmtok Hmtok'") as "[]".
      }
      iDestruct "Hlist2" as (??) "Hl".
      wp_load.
      iMod ("Hclose" with "[Hl' Hlist]") as "_".
      { iNext; iExists _, _; by iFrame. }
      iModIntro.
      wp_pures. wp_bind (CmpXchg _ _ _).
      iInv N as (ℓ'' v'') "(>% & Hl' & [Hlist|Hlist])" "Hclose"; simplify_eq.
      2: {
        iDestruct "Hlist" as "(% & _ & >Hmtok' & _)".
        iDestruct (right_token_exclusive with "Hmtok Hmtok'") as "[]".
      }
      destruct (decide (v'' = (Some l))) as [-> |].
      * iDestruct "Hlist" as "(Hlist & HRest)".
        rewrite is_list_unfold.
        iDestruct "Hlist" as (h' t') "(Hl'' & HP & Hlist) /=".
        wp_cmpxchg_suc.
        iDestruct (mapsto_agree with "Hl'' Hl") as %[= <- <-%oloc_to_val_inj].
        iMod ("Hclose" with "[Hl' Hlist HRest]") as "_".
        { iNext; iExists ℓ'', _. 
          iSplit; first done. iFrame. iLeft.
          by iFrame. }
        iModIntro.
        wp_pures.
        iApply ("HΦ").
        iModIntro. 
        iFrame. iSplit; first done.
        iRight. iExists _.
        iSplit; first done.
        by iFrame.
      * wp_cmpxchg_fail. { destruct v''; simpl; congruence. }
        iMod ("Hclose" with "[Hl' Hlist]") as "_".
        { iNext; iExists ℓ'', _; by iFrame. }
        iModIntro.
        wp_pures.
        iApply ("IH" with "Hmtok HΦ").
  Qed.
End proofs.

Section closed.
  Context `{!heapGS Σ} `{!deferredGpreS Σ} `{!savedPropG Σ} (N : namespace).

  Notation normal_mode_token := left_token.
  Notation deferred_mode_token := right_token.

(* a.d. make sure we can instanitate the thing. *)
  Lemma full_token_split γ :
    own γ (Excl' tt, Excl' tt) ==∗ own γ (Excl' tt, None) ∗ own γ (None, Excl' tt).
  Proof.
    rewrite /left_token /right_token.
    rewrite -own_op.
    rewrite -pair_op.
    iApply own_update.
    rewrite pair_op.
    rewrite cmra_update_updateP.
    apply cmra_updateP_id. 
    done.
  Qed.

  Lemma normal_mode_init P :
    ⊢ |==> ∃ _ : deferredGS Σ, normal_mode P None ∗ deferred_mode_token mode_name.
  Proof using deferredGpreS0 heapGS0 savedPropG0 Σ.
    rewrite /normal_mode.
    iAssert (|==> ∃ γ, own γ (Excl' tt, Excl' tt))%I as ">(%γ1 & Hmode)".
    { 
      iMod (own_alloc (Excl' tt, Excl' tt)) as (γ1) "Hes"; first done. 
      iExists γ1. by iFrame.
    }
    iMod (full_token_split γ1 with "Hmode") as "[Hmode1 Hmode2]".
    iMod (saved_prop_alloc True (DfracOwn 1)) as (γ2) "HglobQ"; first done.
    iMod (own_alloc (● (∅ : gmap (gname * gname) _))) as (γ3) "HI";
      first by rewrite auth_auth_valid.
    iModIntro.
    iExists {| deferred_inG := _; map_name := γ3; mode_name := γ1; deferred_prop_name := γ2 |}.
    iFrame.
    iSplitR "HI".
    - by iApply (is_list_unfold2).
    - rewrite /normal_mode_map.
      iExists ∅. iFrame. done.
  Qed.

  Theorem new_stack_spec :
    {{{ True }}} 
      new_stack #() 
    {{{ s, RET s; ∀ P, 
        |={⊤}=> ∃ _ : deferredGS Σ, is_stack N P s ∗ is_stack_reader N P s }}}.
  Proof using deferredGpreS0 heapGS0 savedPropG0 Σ.
    iIntros (ϕ) "_ Hpost".
    wp_lam.
    wp_alloc ℓ as "Hl".
    iApply "Hpost".
    iIntros "!> %P".
    iMod (normal_mode_init P) as (?) "(Hmode & Hmtok)".
    iMod (inv_alloc N ⊤ (stack_inv P #ℓ) with "[Hmode Hl]") as "#Hinv".
    { iNext. iExists ℓ, None. iFrame.
      done. }
    iModIntro.
    iExists _.
    iFrame.
    by iSplit.
  Qed.

End closed.

