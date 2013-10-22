
defrecord Scrape, started: false, completed: false, root_url: nil
defrecord Page, url: nil, links: [], body: nil, root_url: nil

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
    page = Page.new(url: root_url, root_url: root_url)
    :gen_server.cast(:workqueue, { :add, page })
    { :noreply, Dict.put(state, scrape, []) }
  end

  def handle_cast({ :add_page, page }, state) do
    IO.puts "adding page: #{page.url}"
    :gen_server.cast(:scrapehistory, { :set_processed, page.url })
    scrape = find_scrape(state, page.root_url)
    pages = Dict.get(state, scrape)
    site_links = Enum.filter page.links, &off_root(&1, page.root_url)
    unseen_site_links = Enum.filter site_links, &(!seen(&1))
    Enum.each unseen_site_links, &add_followup_work(&1, page.root_url)
    if(!scrape.started) do
      IO.puts "setting scrape started: #{scrape.root_url}"
      { :noreply,
        Dict.put(Dict.delete(state, scrape), scrape.started(true), [page|pages])}
    else
      { :noreply, Dict.put(scrape, [page|pages]) }
    end
  end

  def handle_call({ :status, root_url }, _from, state) do
    IO.puts "status: #{root_url}"
    scrape = find_scrape(state, root_url)
    IO.puts "scrape: #{scrape.root_url}"
    { :reply, [started: scrape.started, completed: scrape.completed], state }
  end

  def handle_call(:dump, _from, state) do
    { :reply, state, state }
  end

  def find_scrape(state, root_url) do
    Enum.find(Dict.keys(state), fn(s) -> s.root_url == root_url end)
  end

  def add_followup_work(url, root_url) do
    new_page = Page.new(root_url: root_url, url: url)
    :gen_server.cast(:workqueue, { :add, new_page })
  end

  def off_root(url, root_url) do
    true
  end

  def seen(url) do
    :gen_server.call(:scrapehistory, { :contains, url })
  end

end
