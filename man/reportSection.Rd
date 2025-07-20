\name{reportSection}
\alias{reportSection}
\alias{reportSection,character-method}


\title{
Creates a reproducible subsection of the report.
}

\description{
This function creates an object which keeps and manages the contents of a subsection in the report. Each subsection (report-section) may keep both text and a chunk of R codes. The object can be created by users and be added to the report to make the report generation flexible and extensible.
}
\usage{
reportSection(name,title,parent,txt,code_setting,code)
}

\arguments{
  \item{name}{a character which is used as a unique name of the object.}
  \item{title}{a character which specifies the title header of the subsection, appeared in the report}
  \item{parent}{if NULL, the subsection is considered as the level 1, but a subsection can be added as the second or third level of previously added subsections (i.e., subsection of a subsection)}
  \item{txt}{a character or a list with texts.}
  \item{code_setting}{if a chunk of R code is provided in the \code{code} argument, the setting of the chunk can be provided here within a brackt; example: \code{ \{ c(echo=FALSE,results="asis")  \} } }
  \item{code}{a chunk of R code can be provided as an R script within bracket \{   \} }
}
\details{
The name of the subsections should be unique, meaning two subsections can not have the same name. If a new subsection with the same name as previously added subsections is added to the report, the old one will be replaced by the new one.

In the code chuck, to get access the camReport object and its fields or methods, you need to use "object" in the code (see examples).

}
\references{
Ebrahimi et al. (2025) xxx
}
\author{
Elham Ebrahimi
}



\seealso{
camData
}
\examples{
tx <- reportSection(name='introduction',title='Introduction',parent=NULL,txt="This is introduction section...")

tx





# in the following text section, we add an R chunk code which gets access to the camReport object
# through using the object keyword:

tx2 <- reportSection(name='method',title='Methods',parent=NULL,txt="Here, we show the usage of ...",
                      code_setting={c(echo=F,results="asis",warn=F)},
                      code = {
                        
                        object$camera_setup %>%
                        gt() %>%
                        tab_header(
                        title = md("**📷 Camera Deployment Summary**"),
                        subtitle = md("**Table 1.** Details of camera deployments per year")) %>%
                        cols_label(
                        year = "Year",
                        number_camtraps = "Camera Traps",
                        deployment_period = "Deployment Period",
                        setup_period = "Setup Period",
                        pickup_period = "Pickup Period") %>%
                        tab_options(
                        table.font.size = px(14),
                        heading.title.font.size = px(16),
                        heading.subtitle.font.size = px(12),
                        row.striping.include_table_body = TRUE) %>%
                        opt_row_striping() %>%
                        tab_style(style = list(
                        cell_borders(sides = "bottom", color = "gray", weight = px(1))),
                        locations = cells_body()) %>%
                        tab_style(
                        style = list(cell_text(weight = "bold")),locations = cells_column_labels())
                        
                        
                      }
                      )
                      
tx2

}

