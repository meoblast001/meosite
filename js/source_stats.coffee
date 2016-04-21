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

  # Public: Loads data from GitHub using a username.
  #
  # username - Name of user in GitHub.
  # callback - Procedure to call when completed successfully.
  loadData: (username, callback) ->
    this.loadUserRepos username, (repo_ids) =>
      all_code_data = []
      for repo_id in repo_ids
        this.loadRepoCodeStats repo_id, (code_data) =>
          all_code_data.push(code_data)
          this.displayLoadingText(all_code_data.length, repo_ids.length)
          if all_code_data.length == repo_ids.length
            this.computeData(all_code_data)
            callback()

  # Public: Renders the horizontal bar graphs.
  #
  # max_length: Maximum width of a bar.
  # bar_height: Height of a bar.
  render: (max_length, bar_height) ->
    line_counts = (value for key, value of this.code_data)
    max_value = line_counts.
      reduce(((max, cur) -> return if cur > max then cur else max), 0)
    scale_factor = max_length / max_value

    this.loading.hide()
    this.chart.show('slow')

    items_container = this.chart.find('.items')
    for language, lines of this.code_data
      element = $(this.template({ language, lines }))
      element.appendTo(items_container)

      canvas = element.find('.canvas').width(max_length).height(bar_height)
      if canvas.length == 0
        this.loading_text.text(this.error_text)
        return
      context = canvas[0].getContext('2d')
      context.fillStyle = '#000000'
      context.strokeStyle = '#ffffff'
      context.fillRect(0, 0, lines * scale_factor, bar_height * 10)

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

  # Public: Loads from GitHub list of repository full names using a username.
  #
  # username - Name of the GitHub user.
  # callback - Procedure to call when completed with an array of repo full
  #   names.
  loadUserRepos: (username, callback) ->
    repos_url = "https://api.github.com/users/#{username}/repos"
    $.ajax repos_url, {
        dataType: 'json'
        success: (user_data) =>
          repo_full_names =
            (repo.full_name for repo in user_data when !repo.fork)
          callback(repo_full_names)
        error: => this.loading_text.text(this.error_text)
      }

  # Public: Load from GitHub code statistics about a repository.
  #
  # repo_full_name - Full name of GitHub repository.
  # callback - Procedure to call with an object from programming language name
  #   to lines of code written.
  loadRepoCodeStats: (repo_full_name, callback) ->
    code_url = "https://api.github.com/repos/#{repo_full_name}/languages"
    $.ajax code_url, {
        dataType: 'json'
        success: (code_data) => callback(code_data)
        error: => this.loading_text.text(this.error_text)
      }

  # Public: Compute the total amount of lines written in each programming
  #   language from a list of such statistics for each repository.
  #
  # all_code_data - List of objects from programming language name to lines of
  #   code written for each repository.
  #
  # Returns object from programming language names to lines of code written in
  # all repositories.
  computeData: (all_code_data) ->
    this.code_data = {}
    for repo_code_data in all_code_data
      for language, lines of repo_code_data
        if this.code_data[language]
          this.code_data[language] += lines
        else
          this.code_data[language] = lines

window.SourceStats = SourceStats
