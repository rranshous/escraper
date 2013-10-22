defmodule Escraper.PageScraper do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :pagescraper }, __MODULE__, [], [])
  end

  def init(state) do
    { :ok, state }
  end

  def handle_cast(:work_added, state) do
    page = :gen_server.call(:workqueue, :pop)
    spawn_scraper page
    { :noreply, state }
  end

  def spawn_scraper(page) do
    spawn(fn -> scrape_result(do_scrape(page)))
  end

  def do_scrape(page) do
    { :ok, status, headers, c } = :hackney.request(:get, page.url, [], [], [])
    { :ok, body, c2 } = :hackney.body(c)
    page = page.body(body)
  end

end
