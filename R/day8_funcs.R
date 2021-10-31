aoc_day8_dispatch_func <- function(fun, arg, index, global_store, history){
        cmd <- paste0(fun, ":", arg)
        if(cmd %in% get(history, envir = `.GlobalEnv`)) {
                stop("command already run")
        }
        new_hist <- c(get(history, envir = `.GlobalEnv`), cmd)
        assign(history, new_hist, envir = `.GlobalEnv`)
        if(fun == "acc"){
                aoc_day8_accumulator(global_store, arg)
        } else if (fun == "jmp"){
                aoc_day8_jumper(index, arg)
        } else if (fun == "nop") {
                aoc_day8_no_operation(index, arg)
        }
}

aoc_day8_accumulator <- function(store, value ){
        new_value <- get(store, envir = `.GlobalEnv`) + value
        assign(store, new_value, envir = `.GlobalEnv`)
        new_index <- get(index, envir = `.GlobalEnv`) + 1
        assign(index, new_index, envir = `.GlobalEnv`)
}

aoc_day8_jumper <- function(index, value){
        new_index <- get(index, envir = `.GlobalEnv`) + value
        assign(index, new_index,  envir = `.GlobalEnv`)
}

aoc_day8_no_operation <- function(index, value){
        new_index <- get(index, envir = `.GlobalEnv`) + 1
        assign(index, new_index, envir = `.GlobalEnv`)
}
