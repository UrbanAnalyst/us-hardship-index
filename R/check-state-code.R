check_state_code <- function (state) {

    checkmate::assert_character (state, n.chars = 2L)
    state <- toupper (state)

    codes <- data.frame (rbind (
        c ("Alabama", "AL"),
        c ("Kentucky", "KY"),
        c ("Ohio", "OH"),
        c ("Alaska", "AK"),
        c ("Louisiana", "LA"),
        c ("Oklahoma", "OK"),
        c ("Arizona", "AZ"),
        c ("Maine", "ME"),
        c ("Oregon", "OR"),
        c ("Arkansas", "AR"),
        c ("Maryland", "MD"),
        c ("Pennsylvania", "PA"),
        c ("American Samoa", "AS"),
        c ("Massachusetts", "MA"),
        c ("Puerto Rico", "PR"),
        c ("California", "CA"),
        c ("Michigan", "MI"),
        c ("Rhode Island", "RI"),
        c ("Colorado", "CO"),
        c ("Minnesota", "MN"),
        c ("South Carolina", "SC"),
        c ("Connecticut", "CT"),
        c ("Mississippi", "MS"),
        c ("South Dakota", "SD"),
        c ("Delaware", "DE"),
        c ("Missourii", "MO"),
        c ("Tennessee", "TN"),
        c ("District of Columbia", "DC"),
        c ("Montana", "MT"),
        c ("Texas", "TX"),
        c ("Florida", "FL"),
        c ("Nebraska", "NE "),
        c ("Trust Territories", "TT"),
        c ("Georgia", "GA"),
        c ("Nevada", "NV"),
        c ("Utah", "UT"),
        c ("Guam", "GU"),
        c ("New Hampshire", "NH"),
        c ("Vermont", "VT"),
        c ("Hawaii", "HI"),
        c ("New Jersey", "NJ"),
        c ("Virginia", "VA"),
        c ("Idaho", "ID"),
        c ("New Mexico", "NM"),
        c ("Virgin Islands", "VI"),
        c ("Illinois", "IL"),
        c ("New York", "NY"),
        c ("Washington", "WA"),
        c ("Indiana", "IN"),
        c ("North Carolina", "NC"),
        c ("West Virginia", "WV"),
        c ("Iowa", "IA"),
        c ("North Dakota", "ND"),
        c ("Wisconsin", "WI"),
        c ("Kansas", "KS"),
        c ("Northern Mariana Islands", "MP"),
        c ("Wyoming", "WY")
    ))
    names (codes) <- c ("name", "code")

    if (!state %in% codes$code) {
        stop (
            "state [", state, "] is not a US state abbreviation",
            call. = FALSE
        )
    }

    return (state)
}
