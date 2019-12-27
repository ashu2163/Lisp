;;References: Used same structure as in q1. Also kept the basic methods same as q1.
;;			 tutorialspoint.com/lisp
;;			 https://lisp-lang.org/learn/
;;			 https://github.com/Mahendra-Maiti/Missionary-Cannibal-Problem-Solver/blob/master/mcp.lisp
;;			 https://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html
;;			 https://github.com/Mahendra-Maiti/8-puzzle-solver/blob/master/8_puzzle.lisp			 
;;			 https://github.com/jubear720/8-puzzle/blob/master/8puzzle.lsp




;;How to run Code
;; Open clisp 
;; > (load "N_puzzle.lisp")
;; > (8p_solver)

;; To check Feasibility
;; > (is_feasible *start_node* *goal_node*)



(defstruct state
	node 
	g_val
	h_val
	f_val
	parent
)

(defvar *start_node* (list '0 '1 '3 '4 '2 '5 '7 '8 '6 '0))
(defvar *goal_node* (list '1 '2 '3 '4 '5 '6 '7 '8 '0 '8)) 

(defvar *closed_list* nil)

(defun calc_h_val (curr_node)
    (if(eq curr_node NIL)
		(return-from calc_h_val 9999)
    )
    (setf temp 0)
    (dotimes (i 9)
        (if (eq (nth i curr_node)(nth i *goal_node*))
            (setf temp (+ temp 1))
        )
    )
    (return-from calc_h_val (- 9 temp))
)

(defun set_h_val(temp_node)
	(setf h (calc_h_val temp_node))
)

(defun init_state(temp_node)
	(setq new_state (make-state :node temp_node
		:g_val 0
		:h_val ((lambda (temp_node) (setf h (set_h_val temp_node))) temp_node)
		:f_val 9999
		:parent NIL ))

	(return-from init_state new_state)
)

(defun swap (given_node new_pos old_pos)
	(setf copied_node (copy-list given_node) )
	(rotatef (nth new_pos copied_node) (nth old_pos copied_node) )	
	(fill copied_node new_pos :start 9 :end 10)
	(return-from swap copied_node)
)

(defun is_present(given_list node_list)
	(loop for n in node_list do
		(progn
			(if(equal n given_list)(return-from is_present T))
        )

	)

	(return-from is_present nil)

)

(defun generate_Successors(given_state) 
    (setf curr_node (state-node given_state))
    (setf empty_place (tenth curr_node))
    ;;(format t "x: ~a" empty_place)
	(setf move_up (- empty_place 3))
	(setf move_down (+ empty_place 3))
	(setf move_left (- empty_place 1))
	(setf move_right (+ empty_place 1))
	(setf up_child NIL)
	(setf down_child NIL)
	(setf right_child NIL)
	(setf left_child NIL)
	(setf parent_g_val (state-g_val given_state))
	
	(if(>= move_up 0)
		(progn 
			(setf up_child (swap curr_node move_up empty_place))
			;;(print "OK11")
			(setf up_state (init_state up_child))
			(setf (state-g_val up_state) (+ 1 parent_g_val) )
			(setf (state-f_val up_state) (+ (state-g_val up_state) (state-h_val up_state)) )
			(setf (state-parent up_state) given_state)	
		) 
		(progn
			;;(print "OK22")
			(setf up_child NIL) 
			(setf up_state (init_state up_child))
			(setf (state-g_val up_state) 9999 )
			(setf (state-f_val up_state) 9999 )
			(setf (state-parent up_state) NIL)
		)
	)

	(if (<= move_down 8)
		(progn
			;;(print "OK33")
			(setf down_child (swap curr_node move_down empty_place))
			(setf down_state (init_state down_child))
			(setf (state-g_val down_state) (+ 1 parent_g_val))
			(setf (state-f_val down_state) (+ (state-g_val down_state) (state-h_val down_state)))
			(setf (state-parent down_state) given_state)
		)
		(progn
			;;(print "OK44")
			(setf down_child NIL)
			(setf down_state (init_state down_child))
			(setf (state-g_val down_state) 9999)
			(setf (state-f_val down_state) 9999 )
			(setf (state-parent down_state) NIL)
		)
	)

	(if(and (<= move_right 8) (/= (mod (+ 1 empty_place) 3) 0))
		(progn
			;;(print "OK55")
			(setf right_child (swap curr_node move_right empty_place))
			(setf right_state (init_state right_child))
			(setf (state-g_val right_state) (+ 1 parent_g_val))
			(setf (state-f_val right_state) (+ (state-g_val right_state) (state-h_val right_state)))
			(setf (state-parent right_state) given_state)
		)
		(progn
			;;(print "OK66")
			(setf right_child NIL)
			(setf right_state (init_state right_child))
			(setf (state-g_val right_state) 9999)
			(setf (state-f_val right_state) 9999 )
			(setf (state-parent right_state) NIL)
		)
	)

	(if (and (>= move_left 0)(/= (mod (+ empty_place) 3) 0))
		(progn
			;;(print "OK77")
			(setf left_child (swap curr_node move_left empty_place))
			(setf left_state (init_state left_child))
			(setf (state-g_val left_state) (+ 1 parent_g_val))
			(setf (state-f_val left_state) (+ (state-g_val left_state) (state-h_val left_state)))
			(setf (state-parent left_state) given_state)
		)
		(progn
			;;(print "OK88")
			(setf left_child NIL)
			(setf left_state (init_state left_child))
			(setf (state-g_val left_state) 9999)
			(setf (state-f_val left_state) 9999 )
			(setf (state-parent left_state) NIL)
		)
	)
	(setf sons (list up_state down_state left_state right_state))
	;;(format t "Sons: ~a" sons)
)



(defun equal_states(curr_state)
	(setf l1 (subseq (state-node curr_state) 0 9))
	(setf l2 (subseq *goal_node* 0 9))
	(dotimes(i 9)
		(
			if(not(eq (nth i l1)(nth i l2)))
				(return-from equal_states NIL)
		)

	)

	(return-from equal_states T)
	
)
(defun best_state (open_list) 

	(setf min_el (init_state (state-node (nth 0 open_list))))
	(setf (state-f_val min_el) 9999)
	(block list_loop
		(loop for s in open_list do
			(if(< (state-f_val s) (state-f_val min_el))(setf min_el (copy-state s)) () )
		)
		(return-from list_loop min_el)
	)
)


(defun remove_element (el s_list)
	(setq m_list nil)
	(loop for states in s_list do
		(progn
			(if(not(equal (state-node states) (state-node el)))
				(
					push states m_list
				)
			)
			
		)
		collect m_list
	)
	(return-from remove_element m_list)
)

(defun print_path(curr_state states_expanded)
	(format t "~%~%Search successful!!~%")
	(format t "Generated states are: ~% ~%")
	(setf node_list nil)
	(loop while(not(equal curr_state nil)) do
		(progn
			(push (state-node curr_state) node_list)
			(setf curr_state (state-parent curr_state))
		)
	)
	(reverse node_list)
	;;(print_board node_list)
	(loop for x in node_list do
		(setf t_list (subseq x 0 (- (list-length x) 1)))
		(format t "~a ~a ~a ~%" (first x) (second x) (third x))
		(format t "~a ~a ~a ~%" (fourth x) (fifth x) (sixth x))
		(format t "~a ~a ~a ~%" (seventh x) (eighth x) (ninth x))
		(format t "~v@{~A~:*~}~%~%" 20 "-")
	)

	(format t "~%~%Number of nodes expanded is ~a~%~%" states_expanded)

)


(defun A_star (start_state)
    (setf s_node (state-node start_state))
    (format t "~%Start state: ~a" s_node)
	(format t "~%Goal state: ~a ~% ~%" *goal_node*)
    (setf states_expanded 0)
	(setf counter 0)
	
	(if(null start_state)
		(progn
			(format t "~%Failure!!~%")
			(return-from 8p_solver "Failure1")
		)
	)

    (setf open_list (list start_state))
    
    (loop while (not(null open_list)) do
    (progn 
        (setf current (best_state open_list))
        ;;(format t "current: ~a" current)
        (setf counter (+ counter 1))
        (incf states_expanded)    
        (if (equal_states current)
            (progn
			    (print_path current states_expanded)
			    (return-from A_star 'success)
            )
        )
		(setf sons  (generate_Successors current))
		;;(format t "Sons: ~a" sons)
		(setq min_list nil)
		(setf min_val nil)
		(loop for son in sons do
			(progn
			(if (state-node son)
				(if (not (is_present  son *closed_list*))
				(progn	
					(push (state-F_val son) min_list)
			;;(format t "min_f: ~a " min_f)
					(setf min_val (apply 'min min_list))
			;;(format t "min_f-val: ~a " )
				)
				)
			)
			)
		)
		(loop for son in sons do
			(if (state-node son)
				(progn
					(setf son_old_closed (is_present son *closed_list*))
					(if(null son_old_closed)
						(push son open_list))
				)

			)	
			
		)
        (push current *closed_list*) 
		(setf open_list (remove_element current open_list)) 
			   
    )
    
    )

)
;; Reference: https://github.com/Mahendra-Maiti/8-puzzle-solver/blob/master/8_puzzle.lisp
(defun is_feasible (start_node goal_node)
	(setf start_list nil)
	(setf goal_list nil)
	(dotimes(i 9)
		(progn
			(if(not(eq (nth i start_node) 0))
				(progn
					(setf j (+ 1 i))
					(loop while(<= j 8) do
						(progn
							
							(if(not(eq(nth j start_node) 0))
								(progn
									(push (list (nth i start_node) 
													(nth j start_node)) 
														start_list)

								)
							)
							(setf j (+ 1 j))
						)
					)
				)
			)
		)
	)
	
	(dotimes(i 9)
		(progn
			(if(not(eq (nth i goal_node) 0))
				(progn
					(setf j (+ 1 j))
					(loop while(<= j 8) do
						(progn
							
							(if(not(eq(nth j goal_node) 0))
								(progn
									(push (list (nth i goal_node) 
													(nth j goal_node)) 
														goal_list)
								)
							)
							(setf j (+ 1 j))
						)
					)
				)
			)
		)
	)
	(setf common 0)
	(loop for x in start_list do

		(loop for y in goal_list do
			(progn
				(if(and (eq (first x) (first y)) (eq (second x)(second y)) )
					(setf common (+ 1 common))
				)
			)
		)
	)
	(setf inversion_count 0)	
	
	(setf inversion_count (- (list-length start_list) common))
	
	(if(eq (mod inversion_count 2) 0)(return-from is_feasible T)
						(return-from is_feasible NIL))
)




(defun 8p_solver()
	(setq *closed_node* nil)
    (setf start_state (init_state *start_node*))
    (A_star start_state)
) 
