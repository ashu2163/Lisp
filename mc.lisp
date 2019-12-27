;;References: tutorialspoint.com/lisp
;;			  https://lisp-lang.org/learn/
;;			  https://github.com/Mahendra-Maiti/Missionary-Cannibal-Problem-Solver/blob/master/mcp.lisp
;;			  https://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html



;;How to run Code
;; Open clisp
;; > (load "mc.lisp")
;; > (m_c_solver)


(defstruct state
	node 
	g_val
	h_val
	f_val
	parent
)


(defvar *missionary_count* 15) 
(defvar *cannibal_count* 15)	
(defvar *boat_capacity* 6)		
(defvar *goal_node* (list '0 '0 '0 '0 '1))



(defvar *visited_node* nil)

(defun calc_h_val (curr_node)
	(if(eq curr_node NIL)
		(return-from calc_h_val 9999)
	)
	(return-from calc_h_val (float (/ (+ (first curr_node) (second curr_node)) *boat_capacity*)))
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

(defun is_safe_left(given_node)
	(setf m_left (first given_node))
	(setf c_left (second given_node))
	(if(or (< m_left 0) (< c_left 0))(return-from is_safe_left nil))

	(if(and (or (>= m_left c_left) (eq m_left 0)) (<= m_left *missionary_count*) (<= c_left *cannibal_count*))
		(return-from is_safe_left T)
		)

)

(defun is_safe_boat(given_node)
	(setf m_boat (third given_node))
	(setf c_boat(fourth given_node))
	(if(or (< m_boat 0) (< c_boat 0))(return-from is_safe_boat nil))

	(if(and (or (>= m_boat c_boat) (equal m_boat 0)) (<= (+ m_boat c_boat) *boat_capacity*) (>= (+ m_boat c_boat) 1))
		(return-from is_safe_boat T)
	)
)

(defun is_safe_right(given_node)
	(setf m_right (- *missionary_count* (+ (first given_node) (third given_node))))
	(setf c_right (- *cannibal_count* (+ (second given_node) (fourth given_node))))
	(if(or (< m_right 0) (< c_right 0))(return-from is_safe_right nil))
	(if(and (or (>= m_right c_right) (equal m_right 0)) (<= m_right *missionary_count*) (<= c_right *cannibal_count*))
		(return-from is_safe_right T)
	)
	
)

(defun is_safe(given_node)
	(if(and (is_safe_left given_node) (is_safe_boat given_node) (is_safe_right given_node))
		(return-from is_safe T)
		)
)

(defun is_present(given_node node_list)
	(setf first_four (subseq given_node 0 4))
	(loop for n in node_list do
		(progn
			(setf f_four (subseq n 0 4 ))
			(if(equal first_four f_four)(return-from is_present T))
        )

	)

	(return-from is_present nil)

)

(defun is_member (given_state state_list)
	(setf first_four (subseq (state-node given_state) 0 4))
	(loop for n in state_list do
		(progn
			(setf f_four (subseq (state-node n) 0 4 ))
			(if(equal first_four f_four)(return-from is_member T))
		)

	)
	(return-from is_member nil)
)


(defun generate_Successors(given_state)
	(setf boat_pos (fifth (state-node given_state)))
	(setf m_l (first (state-node given_state)))
    ;;(format t "boat_pos: ~a" m_l)
	(setf c_l (second (state-node given_state)))
	(setf m_b (third (state-node given_state)))
	(setf c_b (fourth (state-node given_state)))
	(setf m_r (- *missionary_count* (+ m_l m_b)))
	(setf c_r (- *cannibal_count* (+ c_l c_b)))
	(setf sons nil)
    ;;(format t "Visisted_node: ~a " *visited_node*)
	(if(eq boat_pos 0)
		(progn
			(
				loop for i from 0 to (+ m_l m_b) do
				(progn
                (                    
					loop for j from 0 to (+ c_l c_b) do 
					(progn
						(setf son_node (list (- (+ m_l m_b) i) (- (+ c_l c_b) j) i j '1))
                        ;;(format t "son_node: ~a" son_node)
						( if(and (is_safe son_node) (not(is_present son_node *visited_node*)))
							(progn
								(setf son (init_state son_node))
								(setf (state-parent son) given_state)
								(push son sons)
							)
						)
					)
				)
			)
			)
			
			(return-from generate_Successors sons)
		)
		(progn
			
				(loop for i from 0 to (+ m_b m_r)do
				(progn
				(
					loop for j from 0 to (+ c_b c_r) do
					(progn
						
							(if(not(eq (+ i j) 0)) 

							(progn
								(setf son_node (list m_l c_l (- (+ m_b m_r) i) (- (+ c_b c_r) j) '0))
								( if(and (is_safe son_node) (not (is_present son_node *visited_node*)))
						 			(progn
										(setf son (init_state son_node))
										(setf (state-parent son) given_state)
										(push son sons)
									)
								)
							)
							)

					)
				)	
				)
				)				
			(return-from generate_Successors sons)
			)
	)
)

(defun best_state (open_list) 

	(setf min_el (init_state (state-node (nth 0 open_list))))
    ;;(print min_el)
	(setf (state-h_val min_el) 9999)
	(block list_loop
		(loop for s in open_list do
			(if(< (state-h_val s) (state-h_val min_el))(setf min_el (copy-state s)) () )
		)
		(return-from list_loop min_el)
	)
)

(defun is_near_goal(curr_state)
	(if(eq (state-h_val curr_state) 0.0)
		(return-from is_near_goal T)
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
	(if(not(equal (state-node curr_state) *goal_node*))(push *goal_node* node_list))
	(loop while(not(equal curr_state nil)) do
		(progn
			(push (state-node curr_state) node_list)
			(setf curr_state (state-parent curr_state))
		)
	)

	(loop for n in node_list do
		(progn
			(setf left_bank_config (list (first n) (second n) ))
			(setf boat_config (list (third n) (fourth n)))
			(setf right_bank_config (list (- *missionary_count* (+ (first n) (third n))) (- *cannibal_count* (+ (second n) (fourth n))) ) )
			(format t "m_left: ~a  c_left: ~a  m_boat: ~a  c_boat: ~a  m_right: ~a  c_right: ~a  boat_position: ~a ~%" (first left_bank_config)
												(second left_bank_config) (first boat_config) (second boat_config) 
												(- *missionary_count* (+ (first n) (third n))) (- *cannibal_count* (+ (second n) (fourth n)))
												(fifth n) )

		)
		
	)
	(format t "~%~v@{~A~:*~}~%~%" 70 "-")


)


(defun MC_SOLVER (start_state goal_node)
	(setf *visited_node* nil)
	(format t "Executing Missionary Cannibal problem solver: ~%")
	(setf s_node (state-node start_state))
	(setf g_node goal_node)
	(format t "~%Start state: ~a" s_node)
	(format t "~%Goal state: ~a ~% ~%" goal_node)
	(setf states_expanded 0)
	(setf counter 0)
	(if(null start_state)
		(progn
			(format t "~%Failure!!~%")
			(return-from MC_SOLVER "Failure1")
		)
		
	)
	(setf open_list (list start_state))
	
    ;;(format t "Open_list: ~a" (list-length open_list))
	(loop while (not(null open_list)) do
	(progn
		(setf counter (+ 1 counter))
		(format t "loop count: ~a " counter)
		(setf current (best_state open_list)) 
        (incf states_expanded)
		(if(is_near_goal current) 
			(progn
					
				(print_path current states_expanded)
				(return-from MC_SOLVER "success")
			)
			
		)
		
		(setf sons (generate_Successors current)) 
		(loop for son in sons do
			(progn
				(if(is_near_goal son) 
				(progn
					
					(print_path son states_expanded)
					(return-from MC_SOLVER "success")
				)
			
				)

				( if(state-node son)
					(progn
						(if(and (not(is_present (state-node son) *visited_node*)) (not (is_member son open_list)))
							(push son open_list))
						)						
					)
				)
			)
		)
		(push (state-node current) *visited_node*) 
		(setf open_list (remove_element current open_list)) 
		
	)
	

	(return-from MC_SOLVER "failure2")

	)


(defvar *start_state* (init_state (list *missionary_count* *cannibal_count* '0 '0 '0)))


(defun m_c_solver()
(MC_SOLVER *start_state* *goal_node*) 
)
