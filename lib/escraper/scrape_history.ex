defmodule Escraper.ScrapeHistory do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :scrapehistory }, __MODULE__, [], [])
  end

  def init(history) do
    { :ok, HashSet.new(history) }
  end

  def handle_cast({ :set_processed, url }, history) do
    { :noreply, HashSet.put(history, url) }
  end

  def handle_call({ :contains, url }, _from, history) do
    { :reply, HashSet.member?(history, url), history }
  end

end
