\name{taskwarrior}
\alias{as.data.frame.taskwarrior_tasks}
\alias{read_tasks}
\alias{tags}
\alias{taskwarrior.utils-package}
\alias{taskwarrior.utils}
\title{
  Read Files Exported from Taskwarrior
}
\description{
  Read JSON export files from taskwarrior.
}
\usage{
read_tasks(file, ...)
}
\arguments{
  \item{file}{
    string
  }
  \item{\dots}{
    additional arguments, passed to methods
  }
}
\details{

  Read files produced by \code{taskwarrior export}

}
\value{
  a list with a class attribute \code{taskwarrior_tasks}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  see the Taskwarrior docs
}
\author{
  Enrico Schumann
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
\donttest{
## in a shell
##   task export > ~/Desktop/tasks.json
as.data.frame(read_tasks("~/Desktop/tasks.json"))
}
}
