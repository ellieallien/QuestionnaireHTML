#' Convert an XLSform questionnaire to HTML
#'
#' @param survey.file Path to your questionnaire csv file as character.
#' @param choices.file Path to your choices csv file as character.
#' @param choices.label Choices label column to be used as character.
#' @param survey.label questionnaire label column to be used as character.
#' @param constraint.label sidenote about any constraint related to a question
#' @param output.dir the directory in which to save the output file (absolute path or relative to current working directory)
#' @param output.filename the name of the file. must end in '.html'
#' @param title Name of your questionnaire as character.
#' @param right.to.left Type of the questionnaire writing system (TRUE/FALSE).
#' @param french_excel If TRUE will use read_csv2 and assume csv files are encoded in UTF-8
#' @param special.characters If the text contains non latin characters, specify the language used.
#' @examples
#' questionnaire_to_html("./input/questionnaire_file.csv", "./input/choices_file.csv", "label::Arabic",
#' "label::Arabic", "constraint_message::Arabic", "./output", "survey.html", "Area Based Assessment 2019", TRUE, "arabic")
#'
#' questionnaire_to_html("./input/questionnaire_file.csv", "./input/choices_file.csv", "label",
#' "label", "./", "questionnaire.html", "Area Based Assessment 2019", FALSE, "")
#' @export
questionnaire_to_html <-function(survey.file, choices.file, choices.label, survey.label, constraint.label="", output.dir = "./",
                                 output.filename = "questionnaire.html", title = "", right.to.left = FALSE, special.characters =""){

    # load inputs
    if(!file.exists(survey.file)){
      stop(paste("Survey csv file not found in path",survey.file))
    }

  if(!file.exists(choices.file)){
    stop(paste("Choices file not found in path",choices.file))
  }

   Sys.setlocale("LC_ALL",special.characters)

   if(french_excel = TRUE){
     survey <- read.csv2(survey.file, stringsAsFactors = F, encoding = "UTF-8")
     choices <- read.csv2(choices.file, stringsAsFactors = F, encoding = "UTF-8")

     names(questions)[1] <- "type"
     names(choices)[1] <- "list_name"
   }else{
     survey <- read.csv(survey.file)
     choices <- read.csv(choices.file)

   }

    names(survey) <- sanitise_survey_label(survey.label,names(survey))
    if(constraint.label!="") {
      names(survey) <- sanitise_constraint_message_label(constraint.label,names(survey))
    }
    names(choices) <- sanitise_choices_label_listname(choices.label,names(choices))


    q <- load_questionnaire_without_data(questions.file = survey.file,choices.file = choices.file,choices.label.column.to.use = choices.label)

    inputs <- list(q = q,
                   survey = survey,
                   choices = choices,
                   right.to.left = right.to.left,
                   title=title)


    render_questionnaire_rmd(inputs, dir = output.dir, filename = output.filename)

}

