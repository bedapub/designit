Design <- R6::R6Class("Design",
  public = list(
    samples = NULL,
    batches = NULL,
    seed = NULL,

    new_batch_container = function(
      # arguments
      plate,
      row,
      column,
      interactions=FALSE,
      exclude=NULL,
    ) {
      # make new batch container
      self$batches <- tibble::tibble(
        Batch = 'Batch1'
      )
    },

    distribute_samples = function(seed = NULL) {
      self$seed <- seed
      private$random_seed <- .Random.seed
      randomize(self$batches, self$samples)
    }
  ),

  private = list(
    random_seed = NULL
  )
)
