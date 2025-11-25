// @generated automatically by Diesel CLI.
// Extended with reply threading and moderator support

diesel::table! {
    board_states (id) {
        id -> Integer,
        user_id -> Integer,
        board_id -> Integer,
        last_post_us -> BigInt,
    }
}

diesel::table! {
    boards (id) {
        id -> Integer,
        name -> Text,
        description -> Text,
        created_at_us -> BigInt,
        creator_id -> Nullable<Integer>,  // Owner who can delete (nullable for backwards compat)
    }
}

diesel::table! {
    posts (id) {
        id -> Integer,
        board_id -> Integer,
        user_id -> Integer,
        body -> Text,
        created_at_us -> BigInt,
        parent_id -> Nullable<Integer>,  // For reply threading (null = top-level post)
    }
}

diesel::table! {
    queued_messages (id) {
        id -> Integer,
        sender_id -> Integer,
        recipient_id -> Integer,
        body -> Text,
        created_at_us -> BigInt,
        sent_at_us -> Nullable<BigInt>,
    }
}

diesel::table! {
    users (id) {
        id -> Integer,
        node_id -> Text,
        short_name -> Text,
        long_name -> Text,
        jackass -> Bool,
        in_board -> Nullable<Integer>,
        created_at_us -> BigInt,
        last_seen_at_us -> BigInt,
        last_acted_at_us -> Nullable<BigInt>,
        bio -> Nullable<Text>,
    }
}

diesel::table! {
    moderators (id) {
        id -> Integer,
        user_id -> Integer,
        board_id -> Integer,
        granted_by -> Integer,      // Who granted mod status
        granted_at_us -> BigInt,
    }
}

diesel::joinable!(board_states -> boards (board_id));
diesel::joinable!(board_states -> users (user_id));
diesel::joinable!(posts -> boards (board_id));
diesel::joinable!(posts -> users (user_id));
diesel::joinable!(users -> boards (in_board));
// Note: moderators has multiple FK to users, so we define joins manually in queries

diesel::allow_tables_to_appear_in_same_query!(board_states, boards, posts, queued_messages, users, moderators,);
