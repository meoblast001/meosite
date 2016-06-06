# Public: Fetches information regarding how many lines of code in different
#   languages are written in a user's non-forked repos. Renders a bar graph of
#   these statistics per language.
class SourceStats
  error_text: 'Unfortunately, an error has occurred.'

  # Public: Constructs the object with a DOM element in which the statistics
  #   will be rendered.
  #
  # chart_container - DOM element containing elements of class "loading",
  #   which contains "loading_text", and a an element of class "chart".
  # template_element - DOM element containing Underscore.js template data for
  #   each programming language in the rendered chart. Must contain of canvas of
  #   class "canvas".
  constructor: (chart_container, template_element) ->
    this.chart = $(chart_container).find('.chart')
    this.loading = $(chart_container).find('.loading')
    this.loading_text = this.loading.find('.loading-text')
    this.template = _.template($(template_element).html())
    this.displayLoadingText()

  # Public: Loads cached GitHub JSON data.
  #
  # callback - Procedure to call when completed successfully.
  loadData: (callback) ->
    $.ajax '/code-graph.json', {
        dataType: 'json'
        success: (code_data) =>
          code_data_array = ({language, lines} for language, lines of code_data)
          this.code_data =
            code_data_array.sort((lhs, rhs) -> rhs.lines - lhs.lines)
          callback()
        error: => this.loading_text.text(this.error_text)
      }

  # Public: Renders the horizontal bar graphs.
  render: () ->
    line_counts = (code_datum.lines for code_datum in this.code_data)
    max_value = line_counts.
      reduce(((max, cur) -> return if cur > max then cur else max), 0)
    total_lines = line_counts.reduce(((sum, count) -> sum + count), 0)

    this.loading.hide()
    this.chart.show('slow')

    items_container = this.chart.find('.items')
    for code_datum in this.code_data
      language = code_datum.language
      lines = code_datum.lines

      element = $(this.template({
          language,
          percent: Math.ceil(lines / total_lines * 100)
        }))
      element.appendTo(items_container)

      canvas = element.find('.canvas')
      if canvas.length == 0
        this.loading_text.text(this.error_text)
        return

      scale_factor = canvas.width() / max_value

      context = canvas[0].getContext('2d')
      context.fillStyle = '#000000'
      context.fillRect(0, 0, lines * scale_factor, canvas.height())
      context.strokeStyle = '#ffffff'
      context.lineWidth = 2
      context.strokeRect(0, 0, lines * scale_factor, canvas.height())

  # Public: Displays the loading text, optionally with a "x / y" of completed
  #   elements.
  #
  # completed - Amount of elements completed.
  # total - Amount of elements to be completed.
  displayLoadingText: (completed, total) ->
    text = if completed == undefined
      'Loading data. Please wait.'
    else
      "Loading data (#{completed} / #{total}). Please wait."
    this.loading_text.text(text)

window.SourceStats = SourceStats
