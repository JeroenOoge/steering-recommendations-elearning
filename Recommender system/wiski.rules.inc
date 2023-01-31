<?php

function wiski_rules_event_info() {
  return array(
    'meerkeuzevraag_fout_beantwoord' => array(
      'label' => t('Een meerkeuzevraag werd verkeerd beantwoord'),
      'group' => t('Quiz question'),
      'module' => 'wiski',
      'variables' => array(
        'quiz_result_answer' => array('type' => 'quiz_result_answer', 'label' => t('Quiz result answer')),
      ),
    ),
  );
}

function wiski_rules_action_info() {
  return array(
    'calculate_points_elo_right' => array(
      'label' => t('Calculate points na correcte vraag (Elo)'),
      'group' => t('Quiz'),
      'module' => 'wiski',
      'parameter' => array(
        'vid' => array('type' => 'integer', 'label' => t('vid')),
        'nid' => array('type' => 'integer', 'label' => t('nid')),
        'uid' => array('type' => 'integer', 'label' => t('uid')),
        'question_nid' => array('type' => 'integer', 'label' => t('question_nid')),
        'question_vid' => array('type' => 'integer', 'label' => t('question_vid')),
      ),
      'base' => 'wiski_action_calculate_points_elo_right',
    ),
    'calculate_points_elo_wrong' => array(
      'label' => t('Calculate points na foute vraag (Elo)'),
      'group' => t('Quiz'),
      'module' => 'wiski',
      'parameter' => array(
        'vid' => array('type' => 'integer', 'label' => t('vid')),
        'nid' => array('type' => 'integer', 'label' => t('nid')),
        'uid' => array('type' => 'integer', 'label' => t('uid')),
        'question_nid' => array('type' => 'integer', 'label' => t('question_nid')),
        'question_vid' => array('type' => 'integer', 'label' => t('question_vid')),
      ),
      'base' => 'wiski_action_calculate_points_elo_wrong',
    ),
  );
}

/**
 * Compute new rating (question answered correctly)
 */
function wiski_action_calculate_points_elo_right($vid,$nid,$uid,$question_nid,$question_vid) {
  $level_question = db_query('SELECT level FROM {quiz_question_properties} WHERE vid = :vid AND nid = :nid', array(":vid" => $vid, ":nid" => $nid))->fetchField();
  $level_user = userpoints_get_current_points($uid);

  $in = -($level_user-$level_question)/400;
  $kans_correct = 1/(1+pow(10,$in));
  $new_user = intval($level_user + 0.4 * (1 - $kans_correct) * 400);
  $new_question = intval($level_question + 0.4 * ($kans_correct - 1) * 400);

  $num_updated = db_update('quiz_question_properties')
  ->fields(array(
    'level' => $new_question,
  ))
  ->condition(db_and()
    ->condition('vid', $vid, '=')
    ->condition('nid', $nid, '='))
  ->execute();

  $points = $new_user-$level_user;
  $description = "(question_nid, question_vid): (" . $question_nid . "," . $question_vid . ")"; 

  $params = array(
    'uid' => $uid,
    'points' => $points,
    'tid' => 0,
    'status' => 0,
    'description' => $description,
    'entity_id' => 0,
    'entity_type' => 2,
  );
  $result = userpoints_userpointsapi($params);

  //userpoints_userpointsapi($new_user-$level_user);
}

/**
 * Compute new rating (question answered wrongly)
 */
function wiski_action_calculate_points_elo_wrong($vid,$nid,$uid,$question_nid,$question_vid) {
  $level_question = db_query('SELECT level FROM {quiz_question_properties} WHERE vid = :vid AND nid = :nid', array(":vid" => $vid, ":nid" => $nid))->fetchField();
  $level_user = userpoints_get_current_points($uid);

  $in = -($level_user-$level_question)/400;
  $kans_correct = 1/(1+pow(10,$in));
  $new_user = intval($level_user + 0.4 * (0 - $kans_correct) * 400);
  $new_question = intval($level_question + 0.4 * ($kans_correct - 0) * 400);

  $num_updated = db_update('quiz_question_properties')
  ->fields(array(
    'level' => $new_question,
  ))
  ->condition(db_and()
    ->condition('vid', $vid, '=')
    ->condition('nid', $nid, '='))
  ->execute();

  $points = $new_user-$level_user;
  $description = "(question_nid, question_vid): (" . $question_nid . "," . $question_vid . ")"; 

  $params = array(
    'uid' => $uid,
    'points' => $points,
    'tid' => 0,
    'status' => 0,
    'description' => $description,
    'entity_id' => 0,
    'entity_type' => 2,
  );
  $result = userpoints_userpointsapi($params);

  //userpoints_userpointsapi($new_user-$level_user);
}
