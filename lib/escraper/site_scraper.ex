
defrecord Scrape, started: false, completed: false, root_url: nil, pages: []
defrecord Page, url: nil, links: [], scrape: nil, body: nil

defmodule Escraper.SiteScraper do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :sitescraper }, __MODULE__, [], [])
  end

  def init(state) do
    { :ok, HashDict.new(state) }
  end

  def handle_cast({ :start, root_url }, state) do
    scrape = Scrape.new(root_url: root_url)
    page = Page.new(url: root_url, scrape: scrape)
    :gen_server.cast(:workqueue, { :add, page })
    { :noreply, Dict.put(state, scrape, []) }
  end

  def handle_cast({ :add_page, page }, state) do
    pages = Dict.get(state, page.scrape)
    { :noreply, Dict.put(state, page.scrape, [page|pages]) }
  end

  def handle_call({ :status, root_url }, _from, state) do
    state = Enum.find(Dict.keys(state), fn(s) -> s.root_url == root_url end)
    { :reply, [started: state.started, completed: state.completed], state }
  end

end
