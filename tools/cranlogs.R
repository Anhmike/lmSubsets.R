library("cranlogs")
library("dplyr")
library("ggplot2")

x <- cran_downloads("lmSubsets", from = "2018-03-24")

if (!is.null(x)) {
    op <- par(ask = TRUE)
    on.exit(par(op))

    x <- x %>% mutate(year = format(date, "%Y"), month = format(date, "%m"))
    x <- x %>% group_by(package, year, month) %>% summarize(count = sum(count))
    x <- x %>% group_by(package) %>% mutate(total = cumsum(count))
    x <- x %>% mutate(date = local({
        z <- as.POSIXlt(paste(year, month, "01", sep = "-"))
        z$mon <- z$mon + 1
        z$mday <- z$mday - 1
        as.Date(z)
    }))
    x <- as.data.frame(x %>% select(!c(year, month)))

    z <- ggplot(x, aes(date, count, group=package, color=package)) +
        geom_line() + geom_point(aes(shape=package)) + ggtitle("Monthly downloads")
    print(z)

    z <- ggplot(x, aes(date, total, group=package, color=package)) +
        geom_line() + geom_point(aes(shape=package)) + ggtitle("Total downloads")
    print(z)
}
